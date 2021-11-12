(* This module is a wrapper for running the LIGO lexers as standalone
   pieces of software. *)

(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Std     = Simple_utils.Std
module Config  = Preprocessor.Config
module Options = LexerLib.Options
module Unit    = LexerLib.Unit

(* This module factors the common actions expected from LexerMain in
   all LIGO syntaxes, like reading and checking the command-line,
   building the preprocessor, the lexer, composing and calling them,
   and apply the self pass on the lexical units to obtain tokens. *)

module Make (Config  : Config.S)
            (Options : Options.S)
            (Token   : Token.S)
            (Passes  : Pipeline.PASSES
                       with type lex_unit = Token.t Unit.t) :
  sig
    (* Checking the CLI *)

    type cli_status =
      Ok
    | Info  of string
    | Error of string

    val check_cli : unit -> cli_status

    (* Errors *)

    type 'item error = {
      preprocessed : string option;
      used_items   : 'item list;
      message      : string Region.reg
    }

    (* Lexer for tokens with Menhir in mind *)

    type token = Token.t

    val scan_token : Lexing.lexbuf -> (token, token error) result

    val clear : unit -> unit

    (* Scanning all lexical units, after running the preprocessor if
       specified by [Options]. *)

    type lex_unit = token Unit.t

    type units = lex_unit list

    val scan_all : unit -> Std.t * (units, lex_unit error) result

    (* Filtering tokens *)

    val filter_tokens : units -> token list
  end
