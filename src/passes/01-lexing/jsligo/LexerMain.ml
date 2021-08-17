(* Driver for the JsLIGO lexer *)

module Config           = Preprocessing_jsligo.Config
module PreParams        = Preprocessor.CLI.Make (Config)
module Parameters       = LexerLib.CLI.Make (PreParams)
module Options          = Parameters.Options
module Token            = Lexing_jsligo.Token
module Self_tokens      = Lexing_jsligo.Self_tokens
module MainGen          = Lexing_shared.LexerMainGen
module Main = MainGen.Make (Config) (Options) (Token) (Self_tokens)

let () = Main.check_cli ()
let () = Main.scan_all ()
