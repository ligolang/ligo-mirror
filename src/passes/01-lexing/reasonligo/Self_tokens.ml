(* This module implements a filter on the lexical units of ReasonLIGO
   and produces tokens to be consumed by the parser. *)

(* Vendor dependencies *)

module Core   = LexerLib.Core
module Region = Simple_utils.Region
module Utils  = Simple_utils.Utils
module Trace  = Simple_utils.Trace
module Errors = Lexing_shared.Errors

(* Signature *)

module type S =
  sig
    type token
    type lex_unit = token Core.lex_unit

    type message = string Region.reg

    type lexer_error = Lexing_shared.Errors.t

    val filter :
      raise:lexer_error Trace.raise -> (lex_unit list, message) result -> (token list, message) result
  end

(* Filters *)

let ok x = Stdlib.Ok x

let apply filter = function
  Stdlib.Ok tokens -> filter tokens |> ok
| Error _ as err   -> err

type message = string Region.reg

type token = Token.t
type lex_unit = token Core.lex_unit

type lexer_error = Lexing_shared.Errors.t

(* Virtual token *)

let es6fun = Token.ES6FUN (object 
  method region = Region.ghost
  method attributes = []
  method payload = ""
end)

(* Inserting the ES6FUN virtual token *)

type balancing = 
  B_LBRACE
| B_LPAR

let rec balancing ~(raise : lexer_error Trace.raise) start (balancing_state: balancing list) result tokens =
  let open Token in
  match tokens, balancing_state with 
    LPAR _   as t :: rest, new_balancing_state -> balancing ~raise start (B_LPAR :: new_balancing_state) (t :: result) rest
  | LBRACE _ as t :: rest, new_balancing_state -> balancing ~raise start (B_LBRACE :: new_balancing_state) (t :: result) rest
  | RPAR _   as t :: rest, B_LPAR   :: new_balancing_state -> balancing ~raise start new_balancing_state (t :: result) rest
  | RBRACE _ as t :: rest, B_LBRACE :: new_balancing_state -> balancing ~raise start new_balancing_state (t :: result) rest
  | trigger :: rest, [] -> 
    (trigger :: result), rest
  | [], [] -> result, []
  | (t :: rest), new_balancing_state -> 
    balancing ~raise start new_balancing_state (t :: result) rest
  | [], _ -> 
    let region = Token.to_region start in
    raise.raise (Errors.unbalanced region)

let insert_es6fun ~(raise : lexer_error Trace.raise) tokens =
  let open Token in
  let rec inner result = function
    LPAR _ as hd :: rest ->
      let processed, rest = balancing ~raise hd [B_LPAR] [] rest in
      (match processed with 
        (COLON _ | ARROW _) :: _ -> 
          inner (processed @ (hd :: es6fun :: result)) rest
      | _ -> 
          inner (processed @ (hd :: result)) rest)
  | LBRACE _ as hd :: rest ->
      let processed, rest = balancing ~raise hd [B_LBRACE] [] rest in
      (match processed with 
        (COLON _ | ARROW _) :: _ -> inner (processed @ (hd :: es6fun :: result)) rest
      | _ -> inner (processed @ (hd :: result)) rest)
  | hd :: rest ->
      inner (hd::result) rest
  | [] ->
      List.rev result
  in inner [] tokens

let insert_es6fun ~(raise : lexer_error Trace.raise) = function
  Stdlib.Ok tokens -> insert_es6fun ~raise tokens |> ok
| Error _ as err -> err

(* Filtering out the markup *)

let tokens_of = function
  Stdlib.Ok lex_units ->
    let apply tokens = function
      Core.Token token -> token::tokens
    | Core.Markup _ -> tokens
    | Core.Directive d -> Token.Directive d :: tokens
    in List.fold_left apply [] lex_units |> List.rev |> ok
| Error _ as err -> err

(* Printing tokens *)

let print_token token =
  Printf.printf "%s\n" (Token.to_string ~offsets:true `Point token)

let print_tokens tokens =
  apply (fun tokens -> List.iter print_token tokens; tokens) tokens

(* Exported *)

let filter ~(raise : lexer_error Trace.raise) = Utils.(
  print_tokens
  <@ insert_es6fun ~raise   
  (* <@ print_tokens *)
  <@ tokens_of 
  <@ Style.check)
