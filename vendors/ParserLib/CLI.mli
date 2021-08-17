(* Parsing the command-line options for the parser *)

(* Configuration, options and the parsing status of the latter *)

module type PARAMETERS =
  sig
    module Config  : Preprocessor.Config.S
    module Options : Options.S
    module Status  : module type of Status
  end

(* The instantiation of functor [Make] reads the command line
   interface. *)

module Make (PreParams: LexerLib.CLI.PARAMETERS) : PARAMETERS
