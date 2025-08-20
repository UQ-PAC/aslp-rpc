open Lifter
open Lifter.Opcode

let hex = "0x1234abcd"
let op = of_be_hex_string hex

(** [adr x0, #0x4d2]. R0 = PC + 0x4d2. 0x4d2 = 0b10011010010. *)
let adr_hex = "0x50002680"
let adr_op = of_be_hex_string adr_hex

let%expect_test "opcode basic" =
  print_endline @@ pp @@ op;
  [%expect {| 0x1234abcd |}];
  print_endline @@ to_be_hex_string @@ op;
  [%expect {| 0x1234abcd |}]

let%expect_test "opcode to bytes" =
  print_endline @@ pp_bytes @@ to_be_bytes @@ op;
  [%expect {| 12 34 AB CD |}];
  print_endline @@ pp_bytes @@ to_le_bytes @@ op;
  [%expect {| CD AB 34 12 |}]

let%expect_test "opcode round trip" =
  print_endline @@ pp @@ of_be_hex_string @@ to_be_hex_string op;
  [%expect {| 0x1234abcd |}];
  print_endline @@ pp @@ of_le_bytes @@ to_le_bytes op;
  [%expect {| 0x1234abcd |}];
  print_endline @@ pp @@ of_be_bytes @@ to_be_bytes op;
  [%expect {| 0x1234abcd |}]

let%expect_test "opcode extra cases" =
  (* make sure opcode does not treat "negative" numbers
     differently. *)
  print_endline @@ pp @@ of_be_hex_string "0xfffffff0";
  [%expect {| 0xfffffff0 |}];

  (* test parsing of byte strings *)
  print_endline @@ pp @@ of_le_bytes "\xcd\xab\x34\x12";
  [%expect {| 0x1234abcd |}];
  print_endline @@ pp @@ of_be_bytes "\x12\x34\xab\xcd";
  [%expect {| 0x1234abcd |}]

let pp_result = function
  | Ok xs -> Printf.printf "ok: [%s]\n" @@ String.concat ";\n" @@ List.map asl_stmt_to_string xs
  | Error e -> Printf.printf "err: %s\n" e

let%expect_test "online" =
  CachedOnlineLifter.lift adr_op |> pp_result;
  [%expect {| ok: [Stmt_Assign(LExpr_Array(LExpr_Var("_R"),0),Expr_TApply("add_bits.0",[64],[Expr_Var("_PC");'0000000000000000000000000000000000000000000000000000010011010010']))] |}];

  CachedOnlineLifter.lift ~address:0 adr_op |> pp_result;
  [%expect {| ok: [Stmt_Assign(LExpr_Array(LExpr_Var("_R"),0),'0000000000000000000000000000000000000000000000000000010011010010')] |}]

let%expect_test "offline" =
  OfflineLifter.lift adr_op |> pp_result;
  [%expect {| err: Offline lifter requires opcode address set |}];

  OfflineLifter.lift ~address:0 adr_op |> pp_result;
  [%expect {| ok: [Stmt_Assign(LExpr_Array(LExpr_Var("_R"),0),'0000000000000000000000000000000000000000000000000000010011010010')] |}]

