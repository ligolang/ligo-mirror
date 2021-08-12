(* Parsing the command-line options for the lexer *)

(* General configuration *)

module type CONFIG = module type of Preprocessor.Config

(* CLI options *)

module type OPTIONS = module type of Options

(* Status after parsing CLI options of Preprocessor AND LexerLib *)

module type STATUS = module type of Status

(* Configuration, options and the parsing status of the latter *)

module type PARAMETERS =
  sig
    module Config  : CONFIG
    module Options : OPTIONS
    module Status  : STATUS
  end

(* The instantiation of functor [Make] reads the command line
   interface. *)

module Make (Preprocessor_Params: Preprocessor.CLI.PARAMETERS)
       : PARAMETERS
