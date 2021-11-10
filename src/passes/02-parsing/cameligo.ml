(* This file provides an interface to the CameLIGO parser and
   pretty-printer. *)

(* Vendor dependencies *)

module Trace = Simple_utils.Trace

(* Internal dependencies *)

module Config    = Preprocessing_cameligo.Config
module Token       = Lexing_cameligo.Token
module Self_tokens = Lexing_cameligo.Self_tokens
module ParErr      = Parsing_cameligo.ParErr
module Parser      = Parsing_cameligo.Parser
module CST         = Cst_cameligo.CST
module Pretty      = Parsing_cameligo.Pretty

(* Making the parsers *)

module CameligoParser =
  struct
    module CST = CST
    include Parser

    module Recovery = Parsing_cameligo.RecoverParser
  end

include Parsing_shared.Common.MakeTwoParsers
          (Config) (Token) (ParErr) (Self_tokens) (CST) (CameligoParser)

(* Making the pretty-printers *)

include Parsing_shared.Common.MakePretty (CST) (Pretty)

type raise = Errors.t Trace.raise

let pretty_print_file ~raise buffer file_path =
  ContractParser.parse_file ~raise buffer file_path |> pretty_print

let pretty_print_cst ~raise buffer file_path =
  let cst = parse_file ~raise buffer file_path in
  let buffer = Buffer.create 59 in
  let state =
    Cst_cameligo.Printer.mk_state
      ~offsets:true
      ~mode:`Point
      ~buffer in
  let apply tree = Cst_cameligo.Printer.pp_cst state tree; buffer
  in apply cst
