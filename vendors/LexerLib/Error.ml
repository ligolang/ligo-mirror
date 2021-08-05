(* Lexing errors *)

type error =
  Invalid_utf8_sequence
| Unterminated_comment of string
| Unterminated_string
| Broken_string
| Invalid_character_in_string of char
| Undefined_escape_sequence

type t = error

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
| Invalid_character_in_string c ->
    sprintf "Invalid character %S in string.\n\
             Hint: Remove or replace the character." (Char.escaped c)
