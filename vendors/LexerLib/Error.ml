(* Lexing errors *)

type t =
  Invalid_utf8_sequence
| Unterminated_comment of string
| Unterminated_string
| Broken_string
| Invalid_character_in_string
| Undefined_escape_sequence
| Invalid_linemarker_argument

type error = t

let sprintf = Printf.sprintf

let to_string = function
  Invalid_utf8_sequence ->
    "Invalid UTF-8 sequence."
| Undefined_escape_sequence ->
    "Undefined escape sequence.\n\
     Hint: Remove or replace the sequence."
| Unterminated_string ->
    "Unterminated string.\n\
     Hint: Close with double quotes."
| Unterminated_comment ending ->
    sprintf "Unterminated comment.\n\
             Hint: Close with %S." ending
| Broken_string ->
    "The string starting here is interrupted by a line break.\n\
     Hint: Remove the break, close the string before or insert a \
     backslash."
| Invalid_character_in_string ->
    "Invalid character in string.\n\
     Hint: Remove or replace the character."
| Invalid_linemarker_argument ->
    "Unexpected or invalid linemarker argument.\n\
     Hint: The optional argument is either 1 or 2."
