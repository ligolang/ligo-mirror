(* Driver for the PascaLIGO lexer *)

module Std           = Simple_utils.Std
module Config        = Preprocessing_pascaligo.Config
module PreprocParams = Preprocessor.CLI.Make (Config)
module LexerParams   = LexerLib.CLI.Make (PreprocParams)
module Options       = LexerParams.Options
module Token         = Lexing_pascaligo.Token
module Self_passes   = Lexing_pascaligo_self.Self_passes.Make (Token)
module LexerMainGen  = Lexing_shared.LexerMainGen.Make
                         (Config) (Options) (Token) (Self_passes)

let () =
  let open! LexerMainGen in
  match check_cli () with
    Ok ->
      let Std.{out; err}, _ = scan_all ()
      in Printf.printf  "%s%!" out;
         Printf.eprintf "%s%!" err
  | Info  msg -> Printf.printf "%s\n%!" msg
  | Error msg -> Printf.eprintf "%s\n%!" msg
