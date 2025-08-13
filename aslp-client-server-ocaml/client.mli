(** Client connects immediately to the unix socket supplied by environment
    variable [GTIRB_SEM_SOCKET]

    Otherwise the default [./aslp_rpc_socket]

    Throughout this interface opcodes are provided in big-endian byte-string
    format.

    I.e. the result of Opcode.to_be_bytes. *)

open Aslp_common.Common

type client

val connect : ?socket_fname:string -> unit -> client Lwt.t

val shutdown_server : client -> unit Lwt.t
(** Send server the shutdown message *)

val lift : client -> opcode_be:string -> int -> opcode_sem Lwt.t

val lift_one : client -> opcode_be:string -> int -> opcode_sem
(** Lift a single opcode

    @param opcode_be the opcode in the format "0xffffff" (big endian)
    @param int the pc address of the opcode *)

val lift_multi : client -> opcodes:(string * int) list -> opcode_sem list Lwt.t
(** Lift a batch of (opcode, pc-address) pairs and return a list of the
    corresponding results in order.

    Opcodes are supplied in the 0xffffff big-endian form. *)
