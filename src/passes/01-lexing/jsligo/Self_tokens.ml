(* This module implements a filter on the lexical units of JsLIGO
   and produces tokens to be consumed by the parser. *)

[@@@warning "-42"]

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Markup    = LexerLib.Markup
module Directive = LexerLib.Directive
module Unit      = LexerLib.Unit

(* Signature *)

module type S =
  sig
    type token

    type message = string Region.reg

    val filter :
      (token Unit.t list, message) result -> (token list, message) result
  end

(* Utilities *)

let ok x = Stdlib.Ok x

let apply filter = function
  Stdlib.Ok tokens -> filter tokens |> ok
| Error _ as err   -> err

type message = string Region.reg

type token = Token.t

(* Filtering out the markup *)

let tokens_of = function
  Stdlib.Ok lex_units ->
    let apply tokens = function
      `Token token -> token::tokens
    | `Markup (Markup.BlockCom c) -> Token.BlockCom c :: tokens
    | `Markup (Markup.LineCom c) -> Token.LineCom c :: tokens
    | `Markup _ -> tokens
    | `Directive d -> Token.Directive d :: tokens
    in List.fold_left apply [] lex_units |> List.rev |> ok
| Error _ as err -> err

(* Automatic Semicolon Insertion *)

let automatic_semicolon_insertion tokens =
  let open! Token in
  let rec inner result = function
    (Directive _ as t) :: rest ->
    inner (t :: result) rest
  | (LineCom _ as t) :: rest ->
    inner (t :: result) rest
  | (BlockCom _ as t) :: rest ->
    inner (t :: result) rest
  | (_ as semi) :: (LineCom _ as t) :: rest
  | (_ as semi) :: (BlockCom _ as t) :: rest
  | (SEMI _ as semi) :: (Let _ as t)  :: rest
  | (SEMI _ as semi) :: (Const _ as t)  :: rest
  | (SEMI _ as semi) :: (Type _ as t)  :: rest
  | (SEMI _ as semi) :: (Return _ as t)  :: rest
  | (LBRACE _ as semi) :: (Let _ as t)  :: rest
  | (LBRACE _ as semi) :: (Const _ as t)  :: rest
  | (LBRACE _ as semi) :: (Type _ as t)  :: rest
  | (LBRACE _ as semi) :: (Return _ as t)  :: rest ->
    inner (t:: semi :: result) rest
  | token :: (Const _ as t) :: rest
  | token :: (Type _ as t) :: rest
  | token :: (Return _ as t) :: rest
  | token :: (Let _ as t) :: rest ->
    let (r, _) = Token.proj_token token in
    let (r2, _) = Token.proj_token t in
    if r#stop#line < r2#start#line  then (
      inner (t :: SEMI (Region.make ~start:(r#shift_one_uchar (-1))#stop ~stop:r#stop) :: token :: result) rest
    )
    else (
      match token with
        RBRACE _ as t ->
        inner (t :: SEMI (Region.make ~start:(r#shift_one_uchar (-1))#stop ~stop:r#stop) :: token :: result) rest
      | _ ->
        inner (t :: token :: result) rest
    )
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in
  inner [] tokens

let automatic_semicolon_insertion units =
  apply automatic_semicolon_insertion units

(* Attributes *)

let attribute_regexp = Str.regexp "@\\([a-zA-Z:0-9_]+\\)"

let collect_attributes str =
 let x = Str.full_split attribute_regexp str in
  List.rev (List.fold_left (fun all x ->
    match x with
      Str.Text _ -> all
    | Str.Delim s -> s :: all
  ) [] x)

let attributes tokens =
  let open! Token in
  let rec inner result = function
    LineCom c :: tl
  | BlockCom c :: tl ->
      let attributes = collect_attributes c.Region.value in
      let attributes =
        List.map (fun value -> Attr Region.{value; region = c.region})
                 attributes
      in inner (attributes @ result) tl
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in inner [] tokens

(* OLD?
let collect_attributes str =
  let rec inner result str =
    try (
      let r = Str.search_forward attribute_regexp str 0 in
      let s = Str.matched_group 0 str in
      let s = String.sub s 1 (String.length s - 1) in
      let next = (String.sub str (r + String.length s) (String.length str - (r + + String.length s))) in
      inner (s :: result) next
    )
    with
    | Not_found -> result
  in
  inner [] str

let attributes tokens =
  let open! Token in
  let rec inner result = function
    LineCom c :: tl
  | BlockCom c :: tl ->
      let attributes = collect_attributes c.Region.value in
      let attributes = List.map (fun e ->
        Attr Region.{value = e; region = c.region}) attributes in
      inner (attributes @ result) tl
  | hd :: tl -> inner (hd :: result) tl
  | [] -> List.rev result
  in inner [] tokens
 *)

let attributes units = apply attributes units

(* Injection of Zero-Width Spaces *)

let inject_zwsp lex_units =
  let open! Token in
  let rec aux acc = function
    [] -> List.rev acc
  | (`Token GT _ as gt1)::(`Token GT reg :: _ as units) ->
      aux (`Token (ZWSP reg) :: gt1 :: acc) units
  | unit::units -> aux (unit::acc) units
  in aux [] lex_units

let inject_zwsp units = apply inject_zwsp units

(* DEBUG *)

let unit_to_string = function
  `Token     t -> Token.to_string ~offsets:true `Point t
| `Markup    m -> Markup.to_string ~offsets:true `Point m
| `Directive d -> Directive.to_string ~offsets:true `Point d

let print printer = apply (fun items -> List.iter printer items; items)

let print_unit unit = Printf.printf "%s\n" (unit_to_string unit)

let print_units units = print print_unit units

let print_tokens tokens = print (fun token -> print_unit (`Token token)) tokens

(* COMPOSING FILTERS (exported) *)

let filter units =
     attributes
  @@ automatic_semicolon_insertion
  (*  @@ print_tokens *)
  @@ tokens_of
  (*  @@ print_units *)
  @@ inject_zwsp
  @@ Style.check
     units
