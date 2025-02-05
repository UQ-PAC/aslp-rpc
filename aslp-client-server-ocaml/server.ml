open Aslp_common.Common
open LibASL

module Opcode = struct
  type t = Int32.t

  let to_hex_string (opcode : t) : string = Printf.sprintf "0x%08lx" opcode

  let to_le_bytes (opcode : t) : string =
    let bytes = Bytes.create 4 in
    Bytes.set_int32_le bytes 0 opcode;
    String.of_bytes bytes

  let pp = to_hex_string
  let of_be_bytes (bytes : string) : t = String.get_int32_be bytes 0

  let pp_bytes_le (opcode : t) : string =
    let opcode_le = to_le_bytes opcode in
    let p_byte (b : char) = Printf.sprintf "%02X" (Char.code b) in
    List.of_seq (String.to_seq opcode_le)
    |> List.map p_byte |> String.concat " "
end

module CK = struct
  type t = Opcode.t * int

  let hash = Hashtbl.hash
  let equal x y = x = y
end

module Cache = Vache.Map (Vache.LRU_Sloppy) (Vache.Weak) (CK)

module OpcodeSet = Set.Make (Int32)
(** Client connects automaatically on the first request, to the unix socket
    supplied by environment variable GTIRB_SEM_SOCKET

    Otherwise the default socket is ./aslp_rpc_socket *)

type stats = {
  success : int; (* number of requests which returned instruction semantics *)
  fail : int; (* number of requests which returned a lifter error*)
  total_lifter_calls : int;
      (* number of times the lifter was invoked (cache-misses) *)
  cache_hit_rate : float; (* hit-rate for the in-memory instruction cache *)
  unique_failing_opcodes_le : Opcode.t list;
      (* the list of opcodes which produced lifter errors for lifetime of the server *)
}

module InsnLifter = struct
  (* number of cache misses *)
  let decode_instr_success = ref 0
  let failures = ref OpcodeSet.empty

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
      total_lifter_calls = !cache_misses;
      cache_hit_rate = cache_hit_rate ();
      unique_failing_opcodes_le = OpcodeSet.to_seq !failures |> List.of_seq;
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

  let disas_result ~(opcode : Opcode.t) (f : unit -> Asl_ast.stmt list) :
      (string list, dis_error) result =
    let p_raw a =
      Utils.to_string (Asl_parser_pp.pp_raw_stmt a) |> String.trim
    in
    let opnum_str = Opcode.to_hex_string opcode in
    match f () with
    | res -> Ok (List.map p_raw res)
    | exception exc ->
        if not @@ OpcodeSet.mem opcode !failures then (
          Printf.eprintf
            "error during aslp disassembly (unsupported opcode %s, bytes %s) \
             :: %s\n"
            opnum_str
            (Opcode.pp_bytes_le opcode)
            (Printexc.to_string exc);
          failures := OpcodeSet.add opcode !failures);
        (* Printexc.print_backtrace stderr; *)
        Error { opcode = opnum_str; error = Printexc.to_string exc }

  let to_asli_impl (opcode : Opcode.t) (addr : int) :
      (string list, dis_error) result =
    let address = Some (string_of_int addr) in

    (* below, opnum is the numeric opcode (necessarily BE) and opcode_* are always LE. *)
    (* TODO: change argument of to_asli to follow this convention. *)
    let do_dis () =
      Dis.retrieveDisassembly ?address (Lazy.force env)
        (Dis.build_env (Lazy.force env))
        (Opcode.to_hex_string opcode)
    in
    disas_result ~opcode do_dis

  let aches_cache = Cache.create 5000

  let count_res (r : opcode_sem) =
    (match r with
    | Ok _ -> decode_instr_success := !decode_instr_success + 1
    | Error _ -> decode_instr_fail := !decode_instr_fail + 1);
    r

  let to_asli_cache (opcode : Opcode.t) (addr : int) :
      (string list, dis_error) result =
    let k = (opcode, addr) in
    let x = Cache.find_opt aches_cache k in
    match x with
    | Some x ->
        cache_hits := !cache_hits + 1;
        x
    | None ->
        let v = to_asli_impl opcode addr in
        Cache.replace aches_cache k v;
        cache_misses := !cache_misses + 1;
        v

  let to_asli ?(cache = true) opcode_le addr =
    (if cache then to_asli_cache else to_asli_impl) opcode_le addr |> count_res
end

let lift_opcode ?(cache = true) ~(opcode : Opcode.t) (addr : int) :
    (string list, dis_error) result =
  InsnLifter.to_asli ~cache opcode addr

let lift_opcode_offline_lifter ~(opcode : Opcode.t) (addr : int) :
    (string list, dis_error) result =
  let op = Primops.mkBits 32 (Z.of_int32 opcode) in
  let do_dis () = OfflineASL_pc.Offline.run ~pc:addr op in
  InsnLifter.disas_result ~opcode do_dis |> InsnLifter.count_res

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
            let opcode = Opcode.of_be_bytes opcode_be in
            let lifted : opcode_sem = InsnLifter.to_asli opcode addr in
            let resp : Rpc.msg_resp = Ok lifted in
            Lwt_io.write_value oc resp
        | LiftAll ops ->
            let lifted =
              List.map
                (fun (op, addr) ->
                  InsnLifter.to_asli (Opcode.of_be_bytes op) addr)
                ops
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
