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

val to_string : t -> string
