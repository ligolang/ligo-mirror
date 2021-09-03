(* Preprocessing errors *)

type error =
  Missing_endif
| Dangling_endif
| If_follows_elif
| Else_follows_else
| Dangling_else
| Elif_follows_else
| Dangling_elif
| Missing_space
| Error_directive of string           (* #error ONLY *)
| Invalid_symbol                      (* #define and #undef *)
| File_not_found of string            (* #include #import *)
| Missing_filename                    (* #include #import *)
| Missing_module                      (* #import *)
| Unterminated_comment of string
| Unexpected_argument                 (* #include and #import *)
| Newline_in_string                   (* #include and #import *)
| Unterminated_string of string       (* #include and #import and strings *)
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
| Unterminated_string delimiter ->
    sprintf "The string starting here is not closed.\n\
             Hint: Close it with %S." delimiter
| Dangling_endif ->
    sprintf "Dangling #endif directive.\n\
             Hint: Remove it or add an #if before."
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
             Hint: Remove it or add an #if before."
| Missing_space ->
    sprintf "At least a space character is expected."
| Error_directive msg ->
    if msg = "" then sprintf "Directive #error reached." else msg
| Parse_error ->
    "Parse error in Boolean expression."
| Invalid_symbol ->
   "Invalid symbol."
| File_not_found name ->
    sprintf "File %S not found." name
| Unterminated_comment ending ->
    sprintf "The comment starting here is not closed.\n\
             Hint: Close it with %S." ending
| Missing_filename ->
    sprintf "File name expected in a string literal."
| Missing_module ->
    sprintf "Module name expected in a string literal."
| Unexpected_argument ->
    sprintf "Unexpected argument.\n\
             Hint: Remove it."
| Invalid_character_in_string c ->
    sprintf "Invalid character %S in string.\n\
             Hint: Remove or replace the character." (Char.escaped c)
| Invalid_character c ->
    sprintf "Invalid character '%c' (%d)." c (Char.code c)
