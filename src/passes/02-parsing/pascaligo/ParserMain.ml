(* Driver for the PascaLIGO parser *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Internal dependencies *)

module Config        = Preprocessing_pascaligo.Config
module Token         = Lexing_pascaligo.Token
module Self_tokens   = Lexing_pascaligo.Self_tokens
module CST           = Cst_pascaligo.CST
module ParErr        = Parser_msg
module ParserMainGen = Parsing_shared.ParserMainGen

(* CLIs *)

module PreprocParams = Preprocessor.CLI.Make (Config)
module LexerParams   = LexerLib.CLI.Make (PreprocParams)
module ParserParams  = ParserLib.CLI.Make (LexerParams)
module Options       = ParserParams.Options

(* Renamings on the parser generated by Menhir to suit the functor. *)

module Parser =
  struct
    include Parsing_pascaligo.Parser
    type tree = CST.t

    let main = contract

    module Incremental =
      struct
        let main = Incremental.contract
      end

    module Recovery = Parsing_pascaligo.RecoverParser
  end

module Pretty =
  struct
    include Parsing_pascaligo.Pretty
    type tree = CST.t
  end

module Printer =
  struct
    include Cst_pascaligo.Printer
    type tree = CST.t
  end

(* Finally... *)

module Main = ParserMainGen.Make
                (Config)
                (Options)
                (Token)
                (ParErr)
                (Self_tokens)
                (CST)
                (Parser)
                (Printer)
                (Pretty)

let () = Main.check_cli ()
let () = Main.parse ()
