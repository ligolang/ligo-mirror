(* This file provides an interface to the CameLIGO parser. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies on CameLIGO *)

module File      = Lexer_cameligo.File
module Comments  = Lexer_cameligo.Comments
module Token     = Lexer_cameligo.Token
module Scoping   = Parser_cameligo.Scoping
module ParErr    = Parser_cameligo.ParErr
(*module Printer   = Cst_cameligo.Printer*)

module MakeParser = Shared.Common.MakeParser
module MkParser   = MakeParser (File) (Comments) (Token) (Scoping) (ParErr)

module CST = Cst.Cameligo

(* Parser for contracts *)

module ContractParser_Menhir =
  struct
    include Parser_cameligo.Parser
    type tree = CST.t

    let main = contract

    module Incremental =
      struct
        let main = Incremental.contract
      end
  end

module ContractParser = MkParser (ContractParser_Menhir)

(* Parser for expressions *)

module ExprParser_Menhir =
  struct
    include Parser_cameligo.Parser
    type tree = CST.expr

    let main = interactive_expr

    module Incremental =
      struct
        let main = Incremental.interactive_expr
      end
  end

module ExprParser = MkParser (ExprParser_Menhir)

(* Results and errors *)

type error = Errors.parse_error

type cst    = (CST.t , error) Trace.result
type expr   = (CST.expr, error) Trace.result
type buffer = (Buffer.t , error) Trace.result

let fail msg = Trace.fail @@ Errors.generic msg
type file_path = string
type dirs      = file_path list (* For #include directives *)

(* Calling the parsers *)

let apply = function
  Stdlib.Error msg -> fail msg
| Stdlib.Ok thunk ->
    match thunk () with
      Stdlib.Ok tree -> Trace.ok tree
    | Stdlib.Error msg -> fail msg

(* Parsing contracts *)

let parse_file dirs file_path =
  ContractParser.parse_file dirs file_path |> apply

let parse_program_string dirs string =
  ContractParser.parse_string dirs string |> apply

let parse_program_stdin dirs () =
  ContractParser.parse_channel dirs stdin |> apply

(* Parsing expressions *)

let parse_expression dirs string =
  ExprParser.parse_string dirs string |> apply

(* Preprocessing *)

module MkPreproc = Shared.Common.MakePreproc

let preprocess dirs file_path =
  let module Preproc = MkPreproc (File) (Comments)
  in match Preproc.preprocess dirs file_path with
       Stdlib.Error msg -> fail msg
     | Ok buffer -> Trace.ok buffer

(* Calling the pretty-printers *)

module Pretty    = Parser_cameligo.Pretty
module MkPretty  = Shared.Common.MakePretty
module AllPretty = MkPretty (CST) (Pretty)

let pretty_print            = AllPretty.print_cst
let pretty_print_expression = AllPretty.print_expr
let pretty_print_type_expr  = AllPretty.print_type_expr
let pretty_print_pattern    = AllPretty.print_pattern

let pretty_print_from_source dirs file_path =
  match ContractParser.parse_file dirs file_path with
    Stdlib.Error msg -> fail msg
  | Ok thunk ->
      match thunk () with
        Stdlib.Ok tree -> Trace.ok @@ pretty_print @@ tree
      | Stdlib.Error msg -> fail msg
