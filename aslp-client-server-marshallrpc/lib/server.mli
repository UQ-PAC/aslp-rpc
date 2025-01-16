
open Aslp_common.Common


(**
   Set the address (unix socket filename) to connect to (must be called before first call to lift)
   Overrides the environment variable.
*)
val set_addr : filename:string -> unit


(**
   Start the server listening on the unix domain socket filename supplied by 
   either a call to [set_addr] or environment variable [GTIRB_SEM_SOCKET], or default file [./aslp_rpc_socket].
   
   (will not return until server is shutdown)
*)
val start_server : unit -> unit

(**
  Invoke the lifter directly to lift an opcode
*)
val lift_opcode : ?cache:bool -> opcode_be:string -> int -> opcode_sem
