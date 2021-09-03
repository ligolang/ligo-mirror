(* This module implements a filter on the lexical units of PascaLIGO
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

(* Filtering out the markup *)

let tokens_of = function
  Stdlib.Ok lex_units ->
    let apply tokens = function
      `Token token -> token::tokens
    | `Markup _ -> tokens
    | `Directive d -> Token.Directive d :: tokens
    in List.fold_left apply [] lex_units |> List.rev |> ok
| Error _ as err -> err

(* Exported *)

let filter units = tokens_of units
