open Aslp_common.Common
open Lifter

val set_addr : filename:string -> unit
(** Set the address (unix socket filename) to connect to (must be called before
    first call to lift) Overrides the environment variable. *)

val start_server : unit -> unit
(** Start the server listening on the unix domain socket filename supplied by
    either a call to [set_addr] or environment variable [GTIRB_SEM_SOCKET], or
    default file [./aslp_rpc_socket].

    (will not return until server is shutdown) *)
