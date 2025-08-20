open LibASL

val asl_stmt_to_string : Asl_ast.stmt -> string

module Opcode : sig
  (** Type of opcodes *)

  type t
  (** An Opcode is represented as an opaque type sufficient to hold
      at least 32 bits.

      No other guarantees are given about its internal representation. *)

  val compare : t -> t -> int

  val to_be_hex_string : t -> string
  (** Convert to hex string format [0xffffffff], with padding *)

  val of_be_hex_string : string -> t
  (** Parse from hex string format [0xffffffff] *)

  val to_le_bytes : t -> string
  (** Convert to little-endian bytes string *)

  val to_be_bytes : t -> string
  (** Convert to big-endian bytes string *)

  val pp : t -> string
  (** Default pretty printer for {!Opcode.t}.

      Currently produces the same format as {!to_be_hex_string}. *)

  val pp_bytes : ?sep:string -> string -> string
  (** Formats the given byte string into hexadecimal, with bytes
      separated by the given separator. *)

  val of_le_bytes : string -> t
  (** Parse from little-endian byte string *)

  val of_be_bytes : string -> t
  (** Parse from big-endian byte string *)

  val pp_bytes_le : t -> string
  (** Formats opcode with each byte represented by a pair of hexadecimal digits,
      with bytes in little-endian order and space-separated.

      For example, [A1 B2 C3 D4]. *)
end

module OpcodeSet : Set.S with type elt = Opcode.t

module type Lifter = sig
  val unique_lift_failures : unit -> OpcodeSet.t
  val count_lift_failures : unit -> int
  val count_lift_success : unit -> int
  val lift : ?address:int -> Opcode.t -> (Asl_ast.stmt list, string) result
end

type stats = {
  success : int; (* number of requests which returned instruction semantics *)
  fail : int; (* number of requests which returned a lifter error*)
  total_lifter_calls : int;
      (* number of times the lifter was invoked (cache-misses) *)
  cache_hit_rate : float; (* hit-rate for the in-memory instruction cache *)
  unique_failing_opcodes_le : Opcode.t list;
      (* the list of opcodes which produced lifter errors for lifetime of the server *)
}
(** Type for performance and error rerporting statistics *)

(** Lifter functorised by a caching layer*)
module Cached : functor (L : Lifter) -> sig
  include Lifter

  val cache_hit_rate : unit -> float
  val get_stats : unit -> stats
  val cache_hits : int ref
  val cache_misses : int ref
end

module OnlineLifter : Lifter
(** Lifter using online partial evaluation in aslp *)

(** Lifter using online partial evaluation in aslp with a cache in front *)
module CachedOnlineLifter : sig
  include Lifter

  val cache_hit_rate : unit -> float
  val get_stats : unit -> stats
  val cache_hits : int ref
  val cache_misses : int ref
end

module OfflineLifter : Lifter
(** Pre-compiled lifter code-generated from ahead-of-time partial evaluation.
    Caching is not neccessary due to substantially improved performance. *)
