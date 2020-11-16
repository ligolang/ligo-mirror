(* Driver for the ReasonLIGO parser *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Internal dependencies *)

module Comments = Lexer_reasonligo.Comments
module File     = Lexer_reasonligo.File
module Token    = Lexer_reasonligo.Token
module CST      = Cst.Reasonligo
module ParErr   = Parser_msg

(* CLIs *)

module Preproc_CLI = Preprocessor.CLI.Make (Comments)
module   Lexer_CLI =     LexerLib.CLI.Make (Preproc_CLI)
module  Parser_CLI =    ParserLib.CLI.Make (Lexer_CLI)

(* Renamings on the parser generated by Menhir to suit the functor. *)

module Parser =
  struct
    include Parser_reasonligo.Parser
    type tree = CST.t

    let main = contract

    module Incremental =
      struct
        let main = Incremental.contract
      end
  end

module Pretty =
  struct
    include Parser_reasonligo.Pretty
    type tree = CST.t
  end

module Printer =
  struct
    include Cst_reasonligo.Printer
    type tree = CST.t
  end

(* Finally... *)

module Main = Shared.ParserMainGen.Make
                (Comments)
                (File)
                (Token)
                (CST)
                (Parser)
                (Scoping)
                (ParErr)
                (Printer)
                (Pretty)
                (Parser_CLI)

let () = Main.check_cli ()
let () = Main.parse ()
