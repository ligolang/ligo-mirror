(* Driver for the ReasonLIGO lexer *)

module Std           = Simple_utils.Std
module Config        = Preprocessing_reasonligo.Config
module PreprocParams = Preprocessor.CLI.Make (Config)
module LexerParams   = LexerLib.CLI.Make (PreprocParams)
module Options       = LexerParams.Options
module Token         = Lexing_reasonligo.Token
module Self_tokens   = Lexing_reasonligo.Self_tokens
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
