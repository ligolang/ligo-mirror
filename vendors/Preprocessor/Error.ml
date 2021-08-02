(* Preprocessing errors *)

type error =
  Directive_inside_line
| Missing_endif
| Newline_in_string
| Unterminated_string
| Dangling_endif
| Open_region_in_conditional
| Dangling_endregion
| Conditional_in_region
| If_follows_elif
| Else_follows_else
| Dangling_else
| Elif_follows_else
| Dangling_elif
| Error_directive of string           (* #error ONLY *)
| Parse_error
| Invalid_symbol
| File_not_found of string
| Unterminated_comment of string
| Missing_filename                    (* #include *)
| Unexpected_argument                 (* #include and #import *)

type t = error

let sprintf = Printf.sprintf

let to_string = function
  Directive_inside_line ->
    sprintf "Directive inside a line."
| Missing_endif ->
    sprintf "Missing #endif directive."
| Newline_in_string ->
    sprintf "Invalid newline character in string."
| Unterminated_string ->
    sprintf "Unterminated string.\n\
             Hint: Close with double quotes."
| Dangling_endif ->
    sprintf "Dangling #endif directive.\n\
             Hint: Remove it or add a #if before."
| Open_region_in_conditional ->
    sprintf "Unterminated of #region in conditional.\n\
             Hint: Close with #endregion before #endif."
| Dangling_endregion ->
   sprintf "Dangling #endregion directive.\n\
            Hint: Remove it or use #region before."
| Conditional_in_region ->
    sprintf "Conditional in region.\n\
             Hint: Remove the conditional or the region."
| If_follows_elif ->
    sprintf "Directive #if found in a clause #elif."
| Else_follows_else ->
    sprintf "Directive #else found in a clause #else."
| Dangling_else ->
    sprintf "Directive #else without #if."
| Elif_follows_else ->
    sprintf "Directive #elif found in a clause #else."
| Dangling_elif ->
    sprintf "Dangling #elif directive.\n\
             Hint: Remove it or add a #if before."
| Error_directive msg ->
    if msg = "" then sprintf "Directive #error reached." else msg
| Parse_error ->
    "Parse error in Boolean expression."
| Invalid_symbol ->
   "Expected a symbol (identifier)."
| File_not_found name ->
    sprintf "File %S to include not found." name
| Unterminated_comment ending ->
    sprintf "Unterminated comment.\n\
             Hint: Close with %S." ending
| Missing_filename ->
    sprintf "Filename expected in a string literal."
| Unexpected_argument ->
    sprintf "Unexpected argument."
