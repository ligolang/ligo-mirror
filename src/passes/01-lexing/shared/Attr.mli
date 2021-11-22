(* Attributes *)

module Region = Simple_utils.Region

type key = string

type value = String of string

type attribute = key * value option

type t = attribute

type attributes = attribute Region.reg list

val to_lexeme : attribute -> string
val to_string : attribute -> string
