(* Driver for the Michelson lexer *)

module Config        = Preprocessing_michelson.Config
module PreprocParams = Preprocessor.CLI.Make (Config)
module LexerParams   = LexerLib.CLI.Make (PreprocParams)
module Options       = LexerParams.Options
module Token         = Lexing_michelson.Token
module Self_tokens   = Lexing_michelson.Self_tokens
module MainGen       = Lexing_shared.LexerMainGen
module Main = MainGen.Make (Config) (Options) (Token) (Self_tokens)

let () = Main.check_cli ()
let () = Main.scan_all ()
