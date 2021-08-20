(* Driver for the ReasonLIGO lexer *)

module Config        = Preprocessing_reasonligo.Config
module PreprocParams = Preprocessor.CLI.Make (Config)
module LexerParams   = LexerLib.CLI.Make (PreprocParams)
module Options       = LexerParams.Options
module Token         = Lexing_reasonligo.Token
module Self_tokens   = Lexing_reasonligo.Self_tokens
module MainGen       = Lexing_shared.LexerMainGen
module Main = MainGen.Make (Config) (Options) (Token) (Self_tokens)

let () = Main.check_cli ()
let () = Main.scan_all ()
