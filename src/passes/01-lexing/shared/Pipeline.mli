(* Composing self-passes on lexical units to build a pipeline *)

(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Options = LexerLib.Options
module Unit    = LexerLib.Unit

(* Signature *)

module type S =
  sig
    type lex_unit

    type units = lex_unit list

    (* The type [error] contains the lexical units up to an error,
       whose message is included. *)

    type error = {
      used_units : units;
      message    : string Region.reg
    }

    (* The list of lexical units can be checked and modified by
       [filter]. This function composes a series of passes, which
       depends on the LIGO concrete syntax. The value
       [Options.postprocess] (corresponding to the CLI option
       "--post") specifies the number of passes to be composed. *)

    val filter : units -> (units, error) result
  end

(* Self-passes *)

module type PASSES =
  sig
    type lex_unit

    type units = lex_unit list

    type message = string Region.reg

    val passes : (units -> (units, units * message) result) list
  end

(* Functor *)

module Make (Options : Options.S)
            (Token   : Token.S)
            (Passes  : PASSES with type lex_unit = Token.t Unit.t)
       : S with type lex_unit = Token.t Unit.t
