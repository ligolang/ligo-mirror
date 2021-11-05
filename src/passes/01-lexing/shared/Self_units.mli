(* Vendor dependencies *)

module Region  = Simple_utils.Region
module Options = LexerLib.Options
module Unit    = LexerLib.Unit

(* Signature *)

module type S =
  sig
    type lex_unit

    type units = lex_unit list

    type error = {
      used_units : units;
      message    : string Region.reg
    }

    val filter : units -> (units, error) result
  end

module Make (Options : Options.S) (Token : Token.S)
       : S with type lex_unit = Token.t Unit.t
