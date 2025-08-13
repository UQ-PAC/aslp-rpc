open Aslp_common.Common
open Lifter

type server

(** Provides a server interface for the online lifter *)

val start_server : ?socket_fname:string -> unit -> server Lwt.t
(** Start the server listening on the unix domain socket filename supplied by
    either a call to [set_addr] or environment variable [GTIRB_SEM_SOCKET], or
    default file [./aslp_rpc_socket].

    Returns a promise that completes when the server has started. *)

val shutdown_server : server -> unit Lwt.t
(** Shutdown the server *)

val get_local_lifter_stats : unit -> stats
(** get the lifter statistics for online lifter *)

val run_server : ?socket_fname:string -> unit -> unit
(** Start the server listening on the unix domain socket filename supplied by
    either a call to [set_addr] or environment variable [GTIRB_SEM_SOCKET], or
    default file [./aslp_rpc_socket].

    (will not return until server is shutdown) *)
