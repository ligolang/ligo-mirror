(* Vendor dependencies *)

module Region = Simple_utils.Region
module Unit   = LexerLib.Unit

(* Signature *)

module type S =
  sig
    type token

    type message = string Region.reg

    val filter :
      (token Unit.t list, message) result -> (token list, message) result
  end
