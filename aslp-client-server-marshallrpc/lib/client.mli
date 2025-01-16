(**
   Client connects immediately to the unix socket supplied by environment variable [GTIRB_SEM_SOCKET]

   Otherwise the default [./aslp_rpc_socket]
*)

open Aslp_common.Common

(**
   Set the address (unix socket filename) to connect to (must be called before first call to lift)
   Overrides the environment variable.
*)
val set_addr : filename:string -> unit
 

(**
   Send server the shutdown message
*)
val shutdown_server : unit -> unit Lwt.t

(**
   Lift a single opcode

   @param opcode_be the opcode in the format "0xffffff" (big endian)
   @param int the pc address of the opcode
*)
val lift_one : opcode_be:string -> int -> opcode_sem


(**
   Lift a batch of (opcode, pc-address) pairs and return a list 
   of the corresponding results in order. 

   Opcodes are supplied in the 0xffffff big-endian form.
*)
val lift_multi : opcodes:( (string * int) list) -> opcode_sem list Lwt.t
