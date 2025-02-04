open Aslp_common.Common
open Lwt

let connection = lazy (Lwt_io.open_connection @@ Rpc.sockaddr ())
let set_addr ~filename = Rpc.set_addr filename

let cin () =
  let* ic, _ = Lazy.force connection in
  if Lwt_io.is_closed ic then failwith "connection (in) closed";
  return ic

let cout () =
  let* _, oc = Lazy.force connection in
  if Lwt_io.is_closed oc then failwith "connection (out) closed";
  return oc

let shutdown_server () =
  let* cout = cout () in
  let m : Rpc.msg_call = Shutdown in
  Lwt_io.write_value cout m

let lift ~(opcode_be : string) (addr : int) =
  let* cout = cout () in
  let* cin = cin () in
  let cm : Rpc.msg_call = Lift { opcode_be; addr } in
  let* () = Lwt_io.write_value cout cm in
  let* resp : Rpc.msg_resp = Lwt_io.read_value cin in
  match resp with
  | Ok x -> return x
  | All _ -> Lwt.fail_with "did not expect multi response"

let lift_one ~(opcode_be : string) (addr : int) =
  Lwt_main.run (lift ~opcode_be addr)

let lift_multi ~(opcodes : (string * int) list) : opcode_sem list Lwt.t =
  let* lift_m =
    let* cout = cout () in
    let* cin = cin () in
    let cm : Rpc.msg_call = LiftAll opcodes in
    let* () = Lwt_io.write_value cout cm in
    let* resp : Rpc.msg_resp = Lwt_io.read_value cin in
    match resp with All x -> return x | Ok x -> return [ x ]
  in
  let* _ =
    Lwt_list.iter_s
      (function
        | Ok _ -> Lwt.return ()
        | Error { opcode; error } ->
            Lwt_io.printf "Lift error : %s :: %s\n" opcode error)
      lift_m
  in
  return lift_m
