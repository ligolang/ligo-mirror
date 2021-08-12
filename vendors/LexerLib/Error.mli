(* Lexing errors *)

type error =
  Invalid_utf8_sequence
| Unterminated_comment of string
| Unterminated_string
| Newline_in_string
| Invalid_character_in_string of char
| Undefined_escape_sequence

type t = error

val to_string : t -> string
