(* Static configuration for CameLIGO *)

type block_comment_delimiters = <opening : string; closing : string>
type line_comment_delimiter   = string (* Opening of a line comment *)
type string_delimiter         = string

let block =
  object
    method opening = "(*"
    method closing = "*)"
  end

let block    = Some block
let line     = Some "//"
let string   = Some "\""
let file_ext = Some ".mligo"
