(* This configuration interface gathers information that is not
   provided by the CLI (command-line options), as they are for
   internal use. *)

module type S =
  sig
    type block_comment_delimiters = <opening : string; closing : string>
    type line_comment_delimiter   = string (* Opening of a line comment *)
    type string_delimiter         = string
    type verbatim_delimiters      = <opening : string; closing : string>

    val block    : block_comment_delimiters option
    val line     : line_comment_delimiter option
    val string   : string_delimiter option
    val verbatim : verbatim_delimiters option
    val file_ext : string option (* File extension *)
  end
