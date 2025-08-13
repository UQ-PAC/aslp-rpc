open LibASL

let asl_stmt_to_string a =
  Utils.to_string (Asl_parser_pp.pp_raw_stmt a) |> String.trim

module Opcode = struct
  type t = Int32.t

  let to_le_bytes (opcode : t) : string =
    let bytes = Bytes.create 4 in
    Bytes.set_int32_le bytes 0 opcode;
    String.of_bytes bytes

  let of_le_bytes (bytes : string) : t = String.get_int32_le bytes 0
  let of_be_bytes (bytes : string) : t = String.get_int32_be bytes 0

  let to_be_bytes (o : t) : string =
    let b = Bytes.create 4 in
    Bytes.set_int32_be b 0 o;
    Bytes.to_string b

  let bytes_reverse o = to_le_bytes o |> of_be_bytes
  let of_be_hex_string (s : string) = Int32.of_string s |> bytes_reverse

  let pp_bytes_le (opcode : t) : string =
    let opcode_le = to_le_bytes opcode in
    let p_byte (b : char) = Printf.sprintf "%02X" (Char.code b) in
    List.of_seq (String.to_seq opcode_le)
    |> List.map p_byte |> String.concat " "

  let to_hex_string (opcode : t) : string =
    Printf.sprintf "0x%08lx" (bytes_reverse opcode)

  let pp = to_hex_string

  let%expect_test _ =
    let i = "0x50002680" in
    let op = of_be_hex_string i in
    let leb = to_le_bytes op in
    let oleb = of_le_bytes leb in
    let opbe = to_be_bytes op in
    let rtbe = of_be_bytes opbe in
    let rths = to_hex_string rtbe in
    let reversed = bytes_reverse (bytes_reverse op) in
    Printf.printf "oflebytes %s = %s\n" i (pp oleb);
    Printf.printf "le bytes %s\n" @@ pp_bytes_le op;
    Printf.printf "reverse %s = %s\n" i (to_hex_string reversed);
    Printf.printf "of_hex to_hex %s = %s\n" i (to_hex_string op);
    Printf.printf "%s  = %s\n" (to_hex_string op) rths;
    [%expect
      " \n\
      \ le bytes 50 00 26 80reverse 0x50002680 = 0x50002680\n\
      \ of_hex to_hex 0x50002680 = 0x50002680\n\
      \ 0x50002680  = 0x50002680\n\
      \ "]
end

module OpcodeSet = Set.Make (Int32)

let ( let& ) = Result.bind

module type LifterStat = sig
  val unique_lift_failures : unit -> OpcodeSet.t
  val count_lift_failures : unit -> int
  val count_lift_success : unit -> int
end

module Stats = struct
  (* number of errors *)
  let decode_instr_fail = ref 0
  let failures = ref OpcodeSet.empty

  (* number of cache misses *)
  let decode_instr_success = ref 0

  let count_res r =
    (match r with
    | Ok _ -> decode_instr_success := !decode_instr_success + 1
    | Error _ -> decode_instr_fail := !decode_instr_fail + 1);
    r

  let unique_lift_failures () = !failures
  let count_lift_failures () = !decode_instr_fail
  let count_lift_success () = !decode_instr_success

  let protect f =
    (try Ok (f ()) with e -> Error (Printexc.to_string e)) |> count_res
end

module type Lifter = sig
  include LifterStat

  val lift : ?address:int -> Opcode.t -> (Asl_ast.stmt list, string) result
end

module OfflineLifter : Lifter = struct
  include Stats

  let lift ?(address : int option) (opcode : Opcode.t) :
      (Asl_ast.stmt list, string) result =
    let opcode = Opcode.bytes_reverse opcode in
    let op = Primops.mkBits 32 (Z.of_int32 opcode) in
    let& address =
      Option.to_result ~none:"Offline lifter requires opcode address set"
        address
    in
    protect (fun () -> OfflineASL_pc.Offline.run ~pc:address op)
end

module OnlineLifter : Lifter = struct
  include Stats

  let env =
    lazy
      (Arm_env.aarch64_evaluation_environment ()
      |> Option.to_result
           ~none:
             "unable to load bundled asl files. has aslp been installed \
              correctly?")

  let lift ?(address : int option) (opcode : Opcode.t) :
      (Asl_ast.stmt list, string) result =
    let address = Option.map string_of_int address in
    let& env = Lazy.force env in
    protect (fun () ->
        Dis.retrieveDisassembly ?address env (Dis.build_env env)
          (Opcode.to_hex_string opcode))
end

module CK = struct
  type t = Opcode.t * int option

  let hash = Hashtbl.hash
  let equal x y = x = y
end

module Cache = Vache.Map (Vache.LRU_Sloppy) (Vache.Weak) (CK)

type stats = {
  success : int; (* number of requests which returned instruction semantics *)
  fail : int; (* number of requests which returned a lifter error*)
  total_lifter_calls : int;
      (* number of times the lifter was invoked (cache-misses) *)
  cache_hit_rate : float; (* hit-rate for the in-memory instruction cache *)
  unique_failing_opcodes_le : Opcode.t list;
      (* the list of opcodes which produced lifter errors for lifetime of the server *)
}

module type CachedLifter = sig
  include Lifter

  val cache_hit_rate : unit -> float
  val get_stats : unit -> stats
  val cache_hits : int ref
  val cache_misses : int ref
end

module Cached (L : Lifter) : CachedLifter = struct
  let cache_hits = ref 0
  let cache_misses = ref 0
  let aches_cache = Cache.create 5000

  include L

  let cache_hit_rate () =
    let total = Float.of_int !cache_misses +. Float.of_int !cache_hits in
    if total = 0.0 then 0.0 else Float.of_int !cache_hits /. total

  let get_stats () =
    {
      success = L.count_lift_success ();
      fail = L.count_lift_failures ();
      total_lifter_calls = !cache_misses;
      cache_hit_rate = cache_hit_rate ();
      unique_failing_opcodes_le =
        OpcodeSet.to_seq (L.unique_lift_failures ()) |> List.of_seq;
    }

  let lift ?(address : int option) (opcode : Opcode.t) :
      (Asl_ast.stmt list, string) result =
    let k = (opcode, address) in
    let x = Cache.find_opt aches_cache k in
    match x with
    | Some x ->
        cache_hits := !cache_hits + 1;
        x
    | None ->
        cache_misses := !cache_misses + 1;
        let v = L.lift ?address opcode in
        Cache.replace aches_cache k v;
        v
end

module CachedOnlineLifter = Cached (OnlineLifter)

let%expect_test _ =
  CachedOnlineLifter.lift (Opcode.of_be_hex_string "0x50002680")
  |> Result.get_ok
  |> List.iter (fun i -> asl_stmt_to_string i |> print_endline);
  [%expect
    {|
    Stmt_Assign(LExpr_Array(LExpr_Var("_R"),0),Expr_TApply("add_bits.0",[64],[Expr_Var("_PC");'0000000000000000000000000000000000000000000000000000010011010010']))
  |}]
