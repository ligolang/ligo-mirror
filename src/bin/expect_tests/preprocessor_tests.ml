open Cli_expect

(* PascaLIGO *)

let%expect_test _ =
  run_ligo_bad ["compile" ; "contract"; "../../test/preprocessor/directive_inside_line.ligo"];
  [%expect {|
File "../../test/preprocessor/cameligo/unterminated_string.mligo", line 1, characters 0-1:
  1 | "open string
The string starting here is not closed.
Hint: Close it with "\"".
|}];
