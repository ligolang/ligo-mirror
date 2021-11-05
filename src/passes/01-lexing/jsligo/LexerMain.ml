(* Driver for the JsLIGO lexer *)

module Std           = Simple_utils.Std
module Config        = Preprocessing_jsligo.Config
module PreprocParams = Preprocessor.CLI.Make (Config)
module LexerParams   = LexerLib.CLI.Make (PreprocParams)
module Options       = LexerParams.Options
module Token         = Lexing_jsligo.Token
module Self_tokens   = Lexing_jsligo.Self_tokens
module LexerMainGen  = Lexing_shared.LexerMainGen
module MainGen =
  LexerMainGen.Make (Config) (Options) (Token) (Self_tokens)

let () =
  let open MainGen in
  match check_cli () with
    MainGen.Ok ->
      let Std.{out; err}, _ = scan_all ()
      in Printf.printf  "%s%!" out;
         Printf.eprintf "%s%!" err
  | Info  msg -> Printf.printf "%s\n%!" msg
  | Error msg -> Printf.eprintf "%s\n%!" msg
