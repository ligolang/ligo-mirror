(* This module implements a filter on the lexical units of CameLIGO
   and produces tokens to be consumed by the parser. *)

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

(* Filters *)

let ok x = Stdlib.Ok x

type message = string Region.reg

type token = Token.t

(* Exported *)

let filter units =
     AttachComments.attach
  @@ Style.check
     units
