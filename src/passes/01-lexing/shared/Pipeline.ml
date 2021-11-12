(* This module implements a filter on the lexical units of PascaLIGO
   and produces tokens to be consumed by the parser. *)

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
            (Passes  : PASSES with type lex_unit = Token.t Unit.t) =
  struct
    type lex_unit = Token.t Unit.t

    type units = lex_unit list

    type error = {
      used_units : units;
      message    : string Region.reg
    }

    (* Self-passes *)
(*
    module Style = Style.Make (Token)

    let self_passes = [Style.filter]
 *)

    (* Composing the self-passes *)

    let rec compose up_to index acc = function
      [] -> acc
    | pass::passes ->
        if index <= up_to then
          match acc with
            Ok units ->
              compose up_to (index+1) (pass units) passes
          | Error _ as err -> err
        else acc

    let compose up_to units =
      match compose up_to 1 (Ok units) Passes.passes with
        Ok _ as ok -> ok
      | Error (used_units, message) -> Error {used_units; message}

    let filter : units -> (units, error) result =
      compose (List.length Passes.passes)

(*
    let ok x = Stdlib.Ok x

    let tokens_of = function
      Stdlib.Ok lex_units ->
        let apply tokens = function
            `Token token -> token::tokens
          | `Markup _ -> tokens
          | `Directive d -> Token.Directive d :: tokens
        in List.fold_left apply [] lex_units |> List.rev |> ok
      | Error _ as err -> err
 *)
  end
