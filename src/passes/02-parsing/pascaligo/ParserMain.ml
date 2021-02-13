(* Driver for the PascaLIGO parser *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Internal dependencies *)

module Comments    = Lexer_pascaligo.Comments
module File        = Lexer_pascaligo.File
module Token       = Lexer_pascaligo.Token
module Self_lexing = Lexer_pascaligo.Self_lexing
module CST         = Cst.Pascaligo
module ParErr      = Parser_msg

(* CLIs *)

module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module   Lexer_CLI =     LexerLib.CLI.Make (Preproc_CLI)
module  Parser_CLI =    ParserLib.CLI.Make (Lexer_CLI)

(* Renamings on the parser generated by Menhir to suit the functor. *)

module Parser =
  struct
    include Parser_pascaligo.Parser
    type tree = CST.t

    let main = contract

    module Incremental =
      struct
        let main = Incremental.contract
      end
  end

module Pretty =
  struct
    include Parser_pascaligo.Pretty
    type tree = CST.t
  end

module PrintTokens =
  struct
    include Cst_pascaligo.PrintTokens
    type tree = CST.t
  end

module PrintCST =
  struct
    include Cst_pascaligo.PrintCST
    type tree = CST.t
  end

(* Finally... *)

module Main = Shared_parser.ParserMainGen.Make
                (Comments)
                (File)
                (Token)
                (CST)
                (Parser)
                (ParErr)
                (PrintTokens)
                (PrintCST)
                (Pretty)
                (Parser_CLI)
                (Self_lexing)

let () = Main.check_cli ()
let () = Main.parse ()
