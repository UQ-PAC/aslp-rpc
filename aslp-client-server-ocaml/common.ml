type dis_error = { opcode : string; error : string }
type opcode_sem = (string list, dis_error) result

let ( let* ) = Lwt.bind

module Rpc = struct
  let message_count = ref 0

  let sockfpath =
    ref
    @@
    match Sys.getenv_opt "GTIRB_SEM_SOCKET" with
    | Some x -> x
    | None -> "aslp_rpc_socket"

  let set_addr sockaddr = sockfpath := sockaddr
  let sockaddr () = Lwt_unix.ADDR_UNIX !sockfpath

  type msg_call =
    | Shutdown
    | Lift of { addr : int; opcode_be : string }
    | LiftAll of (string * int) list

  type msg_resp = Ok of opcode_sem | All of opcode_sem list
end
