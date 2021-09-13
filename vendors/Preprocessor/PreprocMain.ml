(* Standalone preprocessor with default settings *)

module CLI = Preprocessor.CLI
module Std = Simple_utils.Std
module PreprocMainGen = Preprocessor.PreprocMainGen

module Config =
  struct
    type block_comment_delimiters = <opening : string; closing : string>
    type line_comment_delimiter   = string (* Opening of a line comment *)
    type string_delimiter         = string
    type verbatim_delimiters      = <opening : string; closing : string>

    let block    = None
    let line     = None
    let string   = Some "\""
    let verbatim = None
    let file_ext = None
  end

module Parameters = CLI.Make (Config)
module Main = PreprocMainGen.Make (Parameters)

let () =
  let open Main in
  match check_cli () with
    Main.Ok ->
      let Std.{out; err}, _ = preprocess ()
      in Printf.printf  "%s%!" out;
         Printf.eprintf "%s%!" err
  | Info  msg -> Printf.printf "%s%!" msg (* Note the absence of "\n" *)
  | Error msg -> Printf.eprintf "%s\n%!" msg
