open Aslp_common.Common
open LibASL
open Lifter

(** Client connects automaatically on the first request, to the unix socket
    supplied by environment variable GTIRB_SEM_SOCKET

    Otherwise the default socket is ./aslp_rpc_socket *)

type server = {
  address : Unix.sockaddr;
  connection : Lwt_io.server;
  shutdown : bool ref;
}

module Server (L : Lifter) = struct
  let disas_result ?(address : int option) (opcode : Opcode.t) :
      (string list, dis_error) result =
    let new_error = not (OpcodeSet.mem opcode (L.unique_lift_failures ())) in
    L.lift ?address opcode
    |> Result.map (List.map asl_stmt_to_string)
    |> Result.map_error (fun e ->
           let opstr = Opcode.to_be_hex_string opcode in
           if new_error then
             Printf.eprintf
               "error during aslp disassembly (unsupported opcode %s, bytes \
                %s) :: %s\n"
               opstr
               (Opcode.pp_bytes_le opcode)
               e;
           { opcode = opstr; error = e })

  let rec respond shutdown (ic : Lwt_io.input_channel)
      (oc : Lwt_io.output_channel) : unit Lwt.t =
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
            let lifted : opcode_sem = disas_result ~address:addr opcode in
            let resp : Rpc.msg_resp = Ok lifted in
            Lwt_io.write_value oc resp
        | LiftAll ops ->
            let lifted =
              List.map
                (fun (op, addr) ->
                  disas_result ~address:addr (Opcode.of_be_bytes op))
                ops
            in
            let resp : Rpc.msg_resp = All lifted in
            Lwt_io.write_value oc resp
      in
      respond shutdown ic oc

  and handle_conn shutdown (_ : Lwt_unix.sockaddr)
      ((ic : Lwt_io.input_channel), (oc : Lwt_io.output_channel)) =
    Lwt.catch
      (fun () -> respond shutdown ic oc)
      (function
        | End_of_file ->
            let* () = Lwt_io.close ic in
            let* () = Lwt_io.close oc in
            Lwt.return ()
        | x -> Lwt_io.printf "%s" (Printexc.to_string x))

  let server address =
    let shutdown = ref false in
    let* s =
      Lwt_io.establish_server_with_client_address address (handle_conn shutdown)
    in
    Lwt.return { address; connection = s; shutdown }

  let rec run_server shutdown () =
    if !shutdown then Lwt.return ()
    else
      let* () =
        Lwt_io.printf "Decoded %d instructions (%d failure) (%d messages)\n"
          (L.count_lift_success ()) (L.count_lift_failures ())
          !Rpc.message_count
      in
      let* () = Lwt_unix.sleep 5.0 in
      run_server shutdown ()

  let start_server ?(socket_fname : string option) () : server Lwt.t =
    let address = Rpc.get_sockaddr ?socket_fname () in
    let* s = server address in
    let* _ =
      Lwt_io.printf "Serving on domain socket GTIRB_SEM_SOCKET=%s\n"
        (Rpc.pp_addr address)
    in
    Lwt_unix.on_signal Sys.sigint (fun _ -> exit 0) |> ignore;
    Lwt_main.at_exit (fun () -> Lwt_io.shutdown_server s.connection);
    Lwt.return s

  let shutdown (s : server) = Lwt_io.shutdown_server s.connection

  let run_server ?(socket_fname : string option) () : unit Lwt.t =
    let* s = start_server ?socket_fname () in
    let* _ = run_server s.shutdown () in
    Lwt_io.shutdown_server s.connection
end

module OnlineServer = Server (CachedOnlineLifter)

let get_local_lifter_stats () = CachedOnlineLifter.get_stats ()

let run_server ?(socket_fname : string option) () =
  Lwt_main.run (OnlineServer.run_server ?socket_fname ())

let start_server ?(socket_fname : string option) () =
  OnlineServer.start_server ?socket_fname ()

let shutdown_server (s : server) = OnlineServer.shutdown s

let%expect_test _ =
  let test =
    let* s = start_server ~socket_fname:"test_socket" () in
    let* _ = shutdown_server s in
    Lwt_io.flush Lwt_io.stdout
  in
  Lwt_main.run test;
  [%expect {| Serving on domain socket GTIRB_SEM_SOCKET=test_socket |}]

let test_lift opcode addr =
  let opcode = Opcode.of_be_hex_string opcode |> Opcode.to_be_bytes in
  let test =
    let* s = start_server () in
    let* client = Client.connect () in
    let* r = Client.lift client ~opcode_be:opcode addr in
    let* _ = shutdown_server s in
    let* _ = Lwt_io.printf "%s\n" (opcode_sem_to_string r) in
    Lwt_io.flush Lwt_io.stdout
  in
  Lwt_main.run test;
  flush_all ()

let%expect_test _ =
  test_lift "0x50002680" 0;
  [%expect
    {|
    Serving on domain socket GTIRB_SEM_SOCKET=aslp_rpc_socket
    Stmt_Assign(LExpr_Array(LExpr_Var("_R"),0),'0000000000000000000000000000000000000000000000000000010011010010')
    |}]

let%expect_test _ =
  test_lift "0x50002680" 123456;
  [%expect
    {|
    Serving on domain socket GTIRB_SEM_SOCKET=aslp_rpc_socket
    Stmt_Assign(LExpr_Array(LExpr_Var("_R"),0),'0000000000000000000000000000000000000000000000011110011100010010')
  |}]

let%expect_test _ =
  test_lift "0x8b031041" 0;
  [%expect
    {|
    Serving on domain socket GTIRB_SEM_SOCKET=aslp_rpc_socket
    Stmt_Assign(LExpr_Array(LExpr_Var("_R"),1),Expr_TApply("add_bits.0",[64],[Expr_Array(Expr_Var("_R"),2);Expr_TApply("append_bits.0",[60;4],[Expr_Slices(Expr_Array(Expr_Var("_R"),3),[Slice_LoWd(0,60)]);'0000'])]))
  |}]

let%expect_test _ =
  let test =
    let* s = start_server ~socket_fname:"aslpsocket" () in
    let* client = Client.connect ~socket_fname:"aslpsocket" () in
    let* r =
      Client.lift client
        ~opcode_be:
          ("0x8b031041" |> Opcode.of_be_hex_string |> Opcode.to_be_bytes)
        0
    in
    let* _ = shutdown_server s in
    let* _ = Lwt_io.printl (opcode_sem_to_string r) in
    Lwt_io.flush Lwt_io.stdout
  in
  Lwt_main.run test;
  [%expect
    {|
    Serving on domain socket GTIRB_SEM_SOCKET=aslpsocket
    Stmt_Assign(LExpr_Array(LExpr_Var("_R"),1),Expr_TApply("add_bits.0",[64],[Expr_Array(Expr_Var("_R"),2);Expr_TApply("append_bits.0",[60;4],[Expr_Slices(Expr_Array(Expr_Var("_R"),3),[Slice_LoWd(0,60)]);'0000'])]))
  |}]
