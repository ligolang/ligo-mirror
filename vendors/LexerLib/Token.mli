(* Tokens *)

type t
type token = t

val is_eof    : token -> bool
val to_region : token -> Simple_utils.Region.t
val to_lexeme : token -> string
val to_string : offsets:bool -> [`Byte | `Point] -> token -> string
