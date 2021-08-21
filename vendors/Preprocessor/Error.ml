(* Preprocessing errors *)

type error =
  Missing_endif
| Dangling_endif
| If_follows_elif
| Else_follows_else
| Dangling_else
| Elif_follows_else
| Dangling_elif
| Error_directive of string           (* #error ONLY *)
| Invalid_symbol                      (* #define and #undef *)
| File_not_found of string            (* #include *)
| Missing_filename                    (* #include *)
| Missing_module                      (* #include *)
| Unterminated_comment of string
| Unexpected_argument                 (* #include and #import *)
| Newline_in_string                   (* #include and #import *)
| Unterminated_string                 (* #include and #import and strings *)
| Invalid_character_in_string of char
| Invalid_character of char           (* #if and #elif *)
| Parse_error                         (* #if and #elif *)

type t = error

let sprintf = Printf.sprintf

let to_string = function
  Missing_endif ->
    sprintf "Missing #endif directive."
| Newline_in_string ->
    (* TODO: When we add quoted strings to LIGO: " or insert a backslash." *)
    sprintf
      "The string starting here is interrupted by a line break.\n\
       Hint: Remove the break or close the string before."
| Unterminated_string ->
    sprintf "Unterminated string.\n\
             Hint: Close with double quotes."
| Dangling_endif ->
    sprintf "Dangling #endif directive.\n\
             Hint: Remove it or add a #if before."
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
    sprintf "File name expected in a string literal."
| Missing_module ->
    sprintf "Module name expected in a string literal."
| Unexpected_argument ->
    sprintf "Unexpected argument."
| Invalid_character_in_string c ->
    sprintf "Invalid character %S in string.\n\
             Hint: Remove or replace the character." (Char.escaped c)
| Invalid_character c ->
    sprintf "Invalid character '%c' (%d)." c (Char.code c)
