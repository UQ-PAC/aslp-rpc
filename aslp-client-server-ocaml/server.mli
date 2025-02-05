open Aslp_common.Common

module Opcode : sig
  (** Type of opcodes *)

  type t = Int32.t

  val to_hex_string : t -> string
  (** Convert to hex string format [0xffffffff] *)

  val to_le_bytes : t -> string
  (** Convert to little-endian bytes string *)

  val pp : t -> string
  (** The same as {! to_hex_string } *)

  val of_be_bytes : string -> t
  (** get opcode from big-endian byte string *)

  val pp_bytes_le : t -> string
  (** Pretty-print {! to_le_bytes} *)
end

val set_addr : filename:string -> unit
(** Set the address (unix socket filename) to connect to (must be called before
    first call to lift) Overrides the environment variable. *)

val start_server : unit -> unit
(** Start the server listening on the unix domain socket filename supplied by
    either a call to [set_addr] or environment variable [GTIRB_SEM_SOCKET], or
    default file [./aslp_rpc_socket].

    (will not return until server is shutdown) *)

type stats = {
  success : int;  (** Number of requests which returned instruction semantics *)
  fail : int;  (** Number of requests which returned a lifter error*)
  total_lifter_calls : int;
      (** Number of times the lifter was invoked (cache-misses) *)
  cache_hit_rate : float;  (** Hit-rate for the in-memory instruction cache *)
  unique_failing_opcodes_le : Opcode.t list;
      (** The list of opcodes which produced lifter errors for lifetime of the
          server *)
}

val get_local_lifter_stats : unit -> stats
(** Return statistics on the number of opcodes lifted through lift_opcode. *)

(** {2 Directly lifting instructions}

    These invoke the lifter directly. *)

val lift_opcode : ?cache:bool -> opcode:Opcode.t -> int -> opcode_sem
(** Invoke the (online) lifter directly to lift an opcode.

    @param cache
      check and update the in-memory cache before lifting this opcode (defaults
      to true). *)

val lift_opcode_offline_lifter : opcode:Opcode.t -> int -> opcode_sem
(** Lift an opcode directly using the offline lifer. This is substantially
    faster but produces more verbose code. *)

