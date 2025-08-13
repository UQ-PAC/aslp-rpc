type dis_error = { opcode : string; error : string }
type opcode_sem = (string list, dis_error) result

let ( let* ) = Lwt.bind

module Rpc = struct
  let message_count = ref 0

  let get_sockaddr ?(socket_fname : string option) () =
    Option.to_list socket_fname
    @ Option.to_list (Sys.getenv_opt "GTIRB_SEM_SOCKET")
    @ [ "aslp_rpc_socket" ]
    |> List.hd
    |> fun a -> Lwt_unix.ADDR_UNIX a

  let pp_addr a =
    match a with
    | Lwt_unix.ADDR_UNIX a -> a
    | Lwt_unix.ADDR_INET (a, b) ->
        Unix.string_of_inet_addr a ^ ":" ^ Int.to_string b

  type msg_call =
    | Shutdown
    | Lift of { addr : int; opcode_be : string }
    | LiftAll of (string * int) list

  type msg_resp = Ok of opcode_sem | All of opcode_sem list
end
