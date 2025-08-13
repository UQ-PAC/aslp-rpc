type dis_error = { opcode : string; error : string }
type opcode_sem = (string list, dis_error) result

let ( let* ) = Lwt.bind
let dis_error_to_string (s : dis_error) = s.opcode ^ " : " ^ s.error

let opcode_sem_to_string (s : opcode_sem) =
  match s with
  | Ok r -> String.concat ";\n" r
  | Error r -> "Lifter error: " ^ dis_error_to_string r

module Rpc = struct
  let message_count = ref 0

  let get_sockaddr ?(socket_fname = "aslp_rpc_socket") () =
    Lwt_unix.ADDR_UNIX
      (match Sys.getenv_opt "GTIRB_SEM_SOCKET" with
      | Some x -> x
      | None -> socket_fname)

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

  let%expect_test _ =
    get_sockaddr ~socket_fname:"hello" () |> pp_addr |> print_endline;
    [%expect "hello"]

  let%expect_test _ =
    get_sockaddr () |> pp_addr |> print_endline;
    [%expect "aslp_rpc_socket"]

  let%expect_test _ =
    Unix.putenv "GTIRB_SEM_SOCKET" "aslpsocket";
    get_sockaddr ~socket_fname:"fname" () |> pp_addr |> print_endline;
    Unix.putenv "GTIRB_SEM_SOCKET" "";
    [%expect "aslpsocket"]

  let%expect_test _ =
    Unix.putenv "GTIRB_SEM_SOCKET" "aslpsocket";
    get_sockaddr () |> pp_addr |> print_endline;
    Unix.putenv "GTIRB_SEM_SOCKET" "";
    [%expect "aslpsocket"]
end
