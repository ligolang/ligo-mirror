(* Driver for the PascaLIGO lexer *)

module Config        = Preprocessing_pascaligo.Config
module PreprocParams = Preprocessor.CLI.Make (Config)
module LexerParams   = LexerLib.CLI.Make (PreprocParams)
module Options       = LexerParams.Options
module Token         = Lexing_pascaligo.Token
module Self_tokens   = Lexing_pascaligo.Self_tokens
module MainGen       = Lexing_shared.LexerMainGen
module Main = MainGen.Make (Config) (Options) (Token) (Self_tokens)

let () = Main.check_cli ()
let () = Main.scan_all ()
