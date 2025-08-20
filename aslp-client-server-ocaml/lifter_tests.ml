open Lifter
open Lifter.Opcode

let hex = "0x5a002680"
let op = of_be_hex_string hex

let%expect_test "opcode basic" =
  print_endline @@ pp @@ op;
  [%expect {| 0x5a002680 |}];
  print_endline @@ to_be_hex_string @@ op;
  [%expect {| 0x5a002680 |}]

let%expect_test "opcode to bytes" =
  print_endline @@ pp_bytes @@ to_be_bytes @@ op;
  [%expect {| 5A 00 26 80 |}];
  print_endline @@ pp_bytes @@ to_le_bytes @@ op;
  [%expect {| 80 26 00 5A |}]

let%expect_test "opcode round trip" =
  print_endline @@ pp @@ of_be_hex_string @@ to_be_hex_string op;
  [%expect {| 0x5a002680 |}];
  print_endline @@ pp @@ of_le_bytes @@ to_le_bytes op;
  [%expect {| 0x5a002680 |}];
  print_endline @@ pp @@ of_be_bytes @@ to_be_bytes op;
  [%expect {| 0x5a002680 |}]

let%expect_test "opcode edge cases" =
  (* make sure opcode does not treat "negative" numbers
     differently. *)
  print_endline @@ pp @@ of_be_hex_string "0xfffffff0";
  [%expect {| 0xfffffff0 |}]

let%expect_test _ =
  CachedOnlineLifter.lift (Opcode.of_be_hex_string "0x50002680")
  |> Result.get_ok
  |> List.iter (fun i -> asl_stmt_to_string i |> print_endline);
  [%expect {| Stmt_Assign(LExpr_Array(LExpr_Var("_R"),0),Expr_TApply("add_bits.0",[64],[Expr_Var("_PC");'0000000000000000000000000000000000000000000000000000010011010010'])) |}]
