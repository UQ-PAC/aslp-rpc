open Aslp_common.Common
open LibASL

module CK = struct
  type t = string * int

  let hash = Hashtbl.hash
  let equal x y = x = y
end

module Cache = Vache.Map (Vache.LRU_Sloppy) (Vache.Weak) (CK)

(** Client connects automaatically on the first request, to the unix socket
    supplied by environment variable GTIRB_SEM_SOCKET

    Otherwise the default socket is ./aslp_rpc_socket *)

type stats = { success : int; fail : int; total : int; cache_hit_rate : float }

module InsnLifter = struct
  (* number of cache misses *)
  let decode_instr_success = ref 0

  (* number of serviced decode requests*)
  let decode_instr_total = ref 0

  (* number of errors *)
  let decode_instr_fail = ref 0
  let cache_hits = ref 0
  let cache_misses = ref 0

  let cache_hit_rate () =
    let total = Float.of_int !cache_misses +. Float.of_int !cache_hits in
    if total = 0.0 then 0.0 else Float.of_int !cache_hits /. total

  let get_stats () =
    {
      success = !decode_instr_success;
      fail = !decode_instr_fail;
      total = !decode_instr_total;
      cache_hit_rate = cache_hit_rate ();
    }

  let env =
    lazy
      (match Arm_env.aarch64_evaluation_environment () with
      | Some e -> e
      | None ->
          Printf.eprintf
            "unable to load bundled asl files. has aslp been installed \
             correctly?";
          exit 1)

  let to_asli_impl (opcode_be : string) (addr : int) :
      (string list, dis_error) result =
    let p_raw a =
      Utils.to_string (Asl_parser_pp.pp_raw_stmt a) |> String.trim
    in
    let p_pretty a = Asl_utils.pp_stmt a |> String.trim in
    let p_byte (b : char) = Printf.sprintf "%02X" (Char.code b) in
    let address = Some (string_of_int addr) in

    (* below, opnum is the numeric opcode (necessarily BE) and opcode_* are always LE. *)
    (* TODO: change argument of to_asli to follow this convention. *)
    let opnum = Int32.to_int String.(get_int32_be opcode_be 0) in
    let opnum_str = Printf.sprintf "0x%08lx" Int32.(of_int opnum) in

    let opcode_list : char list =
      List.(rev @@ of_seq @@ String.to_seq opcode_be)
    in
    let opcode_str = String.concat " " List.(map p_byte opcode_list) in
    let _opcode : bytes = Bytes.of_seq List.(to_seq opcode_list) in

    let do_dis () : (string list * string list, dis_error) result =
      match
        Dis.retrieveDisassembly ?address (Lazy.force env)
          (Dis.build_env (Lazy.force env))
          opnum_str
      with
      | res ->
          decode_instr_success := !decode_instr_success + 1;
          Ok (List.map p_raw res, List.map p_pretty res)
      | exception exc ->
          Printf.eprintf
            "error during aslp disassembly (unsupported opcode %s, bytes %s):\n\n\
             Exception : %s\n"
            opnum_str opcode_str (Printexc.to_string exc);
          decode_instr_fail := !decode_instr_fail + 1;
          (* Printexc.print_backtrace stderr; *)
          Error { opcode = opnum_str; error = Printexc.to_string exc }
    in
    Result.map fst (do_dis ())

  let aches_cache = Cache.create 5000

  let to_asli_cache (opcode_be : string) (addr : int) :
      (string list, dis_error) result =
    let k = (opcode_be, addr) in
    let x = Cache.find_opt aches_cache k in
    match x with
    | Some x ->
        cache_hits := !cache_hits + 1;
        x
    | None ->
        let v = to_asli_impl opcode_be addr in
        Cache.replace aches_cache k v;
        cache_misses := !cache_misses + 1;
        v

  let to_asli ?(cache = true) = if cache then to_asli_cache else to_asli_impl
end

let lift_opcode ?(cache = true) ~(opcode_be : string) (addr : int) :
    (string list, dis_error) result =
  InsnLifter.to_asli ~cache opcode_be addr

module Server = struct
  let shutdown = ref false

  let rec respond (ic : Lwt_io.input_channel) (oc : Lwt_io.output_channel) :
      unit Lwt.t =
    let stop () =
      let* () = Lwt_io.close ic in
      let* () = Lwt_io.close oc in
      Lwt.return ()
    in
    if Lwt_io.is_closed ic || Lwt_io.is_closed oc || !shutdown then stop ()
    else
      let* r : Rpc.msg_call =
        Lwt.catch
          (fun () -> Lwt_io.read_value ic)
          (function
            | exn ->
            let* () = stop () in
            Lwt.fail exn)
      in
      Rpc.message_count := !Rpc.message_count + 1;
      let* () =
        match r with
        | Shutdown ->
            shutdown := true;
            stop ()
        | Lift { addr; opcode_be } ->
            let lifted : opcode_sem = InsnLifter.to_asli opcode_be addr in
            let resp : Rpc.msg_resp = Ok lifted in
            Lwt_io.write_value oc resp
        | LiftAll ops ->
            let lifted =
              List.map (fun (op, addr) -> InsnLifter.to_asli op addr) ops
            in
            let resp : Rpc.msg_resp = All lifted in
            Lwt_io.write_value oc resp
      in
      respond ic oc

  and handle_conn (_ : Lwt_unix.sockaddr)
      ((ic : Lwt_io.input_channel), (oc : Lwt_io.output_channel)) =
    Lwt.catch
      (fun () -> respond ic oc)
      (function
        | End_of_file ->
            let* () = Lwt_io.close ic in
            let* () = Lwt_io.close oc in
            Lwt.return ()
        | x -> Lwt_io.printf "%s" (Printexc.to_string x))

  let server =
    lazy
      (Lwt_io.establish_server_with_client_address (Rpc.sockaddr ()) handle_conn)

  let rec run_server () =
    if !shutdown then Lwt.return ()
    else
      let* () =
        Lwt_io.printf "Decoded %d instructions (%d failure) (%d messages)\n"
          !InsnLifter.decode_instr_success
          !InsnLifter.decode_instr_fail
          !Rpc.message_count
      in
      let* () = Lwt_unix.sleep 5.0 in
      run_server ()

  let start_server () =
    let start =
      let* _ =
        Lwt.return
          (let* _ = Lwt.return (Lazy.force InsnLifter.env) in
           let* m =
             Lwt.return
               (Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) /. 1000000000.0)
           in
           Lwt_io.printf "Initialiesd lifter environment in %f seconds\n" m)
      in
      let* s = Lazy.force server in
      let* _ =
        Lwt_io.printf "Serving on domain socket GTIRB_SEM_SOCKET=%s\n"
          !Rpc.sockfpath
      in

      Lwt_unix.on_signal Sys.sigint (fun _ -> exit 0) |> ignore;

      Lwt_main.at_exit (fun () ->
          print_endline "shutdown server";
          Lwt_io.shutdown_server s);
      run_server ()
    in
    Lwt_main.run start
end

let set_addr ~filename = Rpc.set_addr filename
let start_server () = Server.start_server ()
let get_local_lifter_stats () = InsnLifter.get_stats ()
