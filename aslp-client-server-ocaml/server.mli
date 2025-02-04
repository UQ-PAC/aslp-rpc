open Aslp_common.Common

val set_addr : filename:string -> unit
(** Set the address (unix socket filename) to connect to (must be called before
    first call to lift) Overrides the environment variable. *)

val start_server : unit -> unit
(** Start the server listening on the unix domain socket filename supplied by
    either a call to [set_addr] or environment variable [GTIRB_SEM_SOCKET], or
    default file [./aslp_rpc_socket].

    (will not return until server is shutdown) *)

val lift_opcode : ?cache:bool -> opcode_be:string -> int -> opcode_sem
(** Invoke the lifter directly to lift an opcode *)

type stats = { success : int; fail : int; total : int; cache_hit_rate : float }

(**
   Return statistics on the number of opcodes lifted through lift_opcode.
*)
val get_local_lifter_stats : unit -> stats
