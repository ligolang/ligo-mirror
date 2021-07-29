(* Vendor dependencies *)

module State  = LexerLib.State
module Region = Simple_utils.Region

(* Signature *)

module type S =
  sig
    type token
    type lex_unit = token State.lex_unit

    type message = string Region.reg

    val filter :
      (lex_unit list, message) result -> (token list, message) result
  end
