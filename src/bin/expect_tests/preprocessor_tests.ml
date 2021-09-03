open Cli_expect

(* PascaLIGO *)

let%expect_test _ =
  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/pascaligo/dangling_elif.ligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/dangling_elif.ligo", line 1, characters 0-5:
  1 | #elif a
Dangling #elif directive.
Hint: Remove it or add an #if before.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/pascaligo/dangling_else.ligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/dangling_else.ligo", line 1, characters 0-5:
  1 | #else
Directive #else without #if.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/pascaligo/dangling_endif.ligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/dangling_endif.ligo", line 1, characters 0-6:
  1 | #endif
Dangling #endif directive.
Hint: Remove it or add an #if before.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/pascaligo/elif_follows_else.ligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/elif_follows_else.ligo", line 3, characters 0-5:
  2 | #else
  3 | #elif b
Directive #elif found in a clause #else.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/pascaligo/else_follows_else.ligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/else_follows_else.ligo", line 3, characters 0-5:
  2 | #else
  3 | #else
Directive #else found in a clause #else.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/pascaligo/error_directive1.ligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/error_directive1.ligo", line 1, characters 0-6:
  1 | #error
Directive #error reached.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/pascaligo/error_directive2.ligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/error_directive2.ligo", line 1, characters 0-6:
  1 | #error Stopping here.
  2 | This should not be copied to the output
Stopping here.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/pascaligo/file_not_found.ligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/file_not_found.ligo", line 1, characters 9-24:
  1 | #include "foobarbaz.txt"
File "foobarbaz.txt" not found.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/pascaligo/if_follows_elif.ligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/if_follows_elif.ligo", line 3, characters 0-3:
  2 | #elif b
  3 | #if c
Directive #if found in a clause #elif.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/pascaligo/invalid_character_in_string.ligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/invalid_character_in_string.ligo", line 1, characters 27-28:
  1 | "containing a tabulation ->\t<-"
Invalid character "\\t" in string.
Hint: Remove or replace the character.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/pascaligo/invalid_character.ligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/invalid_character.ligo", line 1, characters 4-5:
  1 | #if $
  2 | #endif
Invalid character '$' (36).
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/pascaligo/invalid_symbol.ligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/invalid_symbol.ligo", line 1, characters 8-9:
  1 | #define _
Invalid symbol.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/pascaligo/missing_endif.ligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/missing_endif.ligo", line 2, character 0:
  1 | #if false
Missing #endif directive.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/pascaligo/missing_filename.ligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/missing_filename.ligo", line 1, characters 7-8:
  1 | #import\n
File name expected in a string literal.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/pascaligo/missing_module.ligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/missing_module.ligo", line 1, characters 24-25:
  1 | #import "included.ligo" M
Module name expected in a string literal.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/pascaligo/missing_space.ligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/missing_space.ligo", line 1, characters 7-8:
  1 | #defineC
At least a space character is expected.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/pascaligo/newline_in_string.ligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/newline_in_string.ligo", line 1, characters 0-1:
  1 | "broken string
The string starting here is interrupted by a line break.
Hint: Remove the break or close the string before.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/pascaligo/parse_error.ligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/parse_error.ligo", line 1, characters 6-7:
  1 | #if A ! = B
  2 | #endif
Parse error in Boolean expression.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/pascaligo/unexpected_argument.ligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/unexpected_argument.ligo", line 1, characters 25-26:
  1 | #include "included.ligo" "M" C
Unexpected argument.
Hint: Remove it.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/pascaligo/unterminated_comment.ligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/unterminated_comment.ligo", line 1, characters 0-2:
  1 | (* Unterminated
  2 |    comment
The comment starting here is not closed.
Hint: Close it with "*)".
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/pascaligo/unterminated_string.ligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/pascaligo/unterminated_string.ligo", line 1, characters 0-1:
  1 | "open string
The string starting here is not closed.
Hint: Close it with "\"".
|}];

(* CameLIGO *)

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/cameligo/dangling_elif.mligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/dangling_elif.mligo", line 1, characters 0-5:
  1 | #elif a
Dangling #elif directive.
Hint: Remove it or add an #if before.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/cameligo/dangling_else.mligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/dangling_else.mligo", line 1, characters 0-5:
  1 | #else
Directive #else without #if.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/cameligo/dangling_endif.mligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/dangling_endif.mligo", line 1, characters 0-6:
  1 | #endif
Dangling #endif directive.
Hint: Remove it or add an #if before.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/cameligo/elif_follows_else.mligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/elif_follows_else.mligo", line 3, characters 0-5:
  2 | #else
  3 | #elif b
Directive #elif found in a clause #else.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/cameligo/else_follows_else.mligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/else_follows_else.mligo", line 3, characters 0-5:
  2 | #else
  3 | #else
Directive #else found in a clause #else.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/cameligo/error_directive1.mligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/error_directive1.mligo", line 1, characters 0-6:
  1 | #error
Directive #error reached.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/cameligo/error_directive2.mligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/error_directive2.mligo", line 1, characters 0-6:
  1 | #error Stopping here.
  2 | This should not be copied to the output
Stopping here.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/cameligo/file_not_found.mligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/file_not_found.mligo", line 1, characters 9-24:
  1 | #include "foobarbaz.txt"
File "foobarbaz.txt" not found.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/cameligo/if_follows_elif.mligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/if_follows_elif.mligo", line 3, characters 0-3:
  2 | #elif b
  3 | #if c
Directive #if found in a clause #elif.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/cameligo/invalid_character_in_string.mligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/invalid_character_in_string.mligo", line 1, characters 27-28:
  1 | "containing a tabulation ->\t<-"
Invalid character "\\t" in string.
Hint: Remove or replace the character.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/cameligo/invalid_character.mligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/invalid_character.mligo", line 1, characters 4-5:
  1 | #if $
  2 | #endif
Invalid character '$' (36).
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/cameligo/invalid_symbol.mligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/invalid_symbol.mligo", line 1, characters 8-9:
  1 | #define _
Invalid symbol.
|}];

  run_ligo_bad ["compile-contract";
                "../../test/preprocessor/cameligo/missing_endif.mligo";
                "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/missing_endif.mligo", line 2, character 0:
  1 | #if false
Missing #endif directive.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/cameligo/missing_filename.mligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/missing_filename.mligo", line 1, characters 7-8:
  1 | #import\n
File name expected in a string literal.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/cameligo/missing_module.mligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/missing_module.mligo", line 1, characters 25-26:
  1 | #import "included.mligo" M
Module name expected in a string literal.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/cameligo/missing_space.mligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/missing_space.mligo", line 1, characters 7-8:
  1 | #defineC
At least a space character is expected.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/cameligo/newline_in_string.mligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/newline_in_string.mligo", line 1, characters 0-1:
  1 | "broken string
The string starting here is interrupted by a line break.
Hint: Remove the break or close the string before.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/cameligo/parse_error.mligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/parse_error.mligo", line 1, characters 6-7:
  1 | #if A ! = B
  2 | #endif
Parse error in Boolean expression.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/cameligo/unexpected_argument.mligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/unexpected_argument.mligo", line 1, characters 26-27:
  1 | #include "included.mligo" "M" C
Unexpected argument.
Hint: Remove it.
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/cameligo/unterminated_comment.mligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/unterminated_comment.mligo", line 1, characters 0-2:
  1 | (* Unterminated
  2 |    comment
The comment starting here is not closed.
Hint: Close it with "*)".
|}];

   run_ligo_bad ["compile-contract";
                 "../../test/preprocessor/cameligo/unterminated_string.mligo";
                 "main"];
  [%expect {|
File "../../test/preprocessor/cameligo/unterminated_string.mligo", line 1, characters 0-1:
  1 | "open string
The string starting here is not closed.
Hint: Close it with "\"".
|}];
