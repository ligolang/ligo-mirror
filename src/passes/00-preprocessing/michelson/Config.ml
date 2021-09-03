(* Static configuration for Michelson *)

type block_comment_delimiters = <opening : string; closing : string>
type line_comment_delimiter   = string (* Opening of a line comment *)
type string_delimiter         = string

let block    = None
let line     = Some "#"
let string   = Some "\""
let file_ext = Some ".tz"
