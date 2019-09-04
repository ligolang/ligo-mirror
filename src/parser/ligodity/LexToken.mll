{
type lexeme = string

let sprintf = Printf.sprintf

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module SMap   = Utils.String.Map
module SSet   = Utils.String.Set

(* TOKENS *)

type t =
  (* Symbols *)

  ARROW of Region.t     (* "->" *)
| CONS of Region.t      (* "::" *)
| CAT of Region.t       (* "^"  *)
  (*| APPEND   (* "@"  *)*)

  (* Arithmetics *)

| MINUS of Region.t     (* "-" *)
| PLUS of Region.t      (* "+" *)
| SLASH of Region.t     (* "/" *)
| TIMES of Region.t     (* "*" *)

  (* Compounds *)

| LPAR of Region.t      (* "(" *)
| RPAR of Region.t      (* ")" *)
| LBRACKET of Region.t  (* "[" *)
| RBRACKET of Region.t  (* "]" *)
| LBRACE of Region.t    (* "{" *)
| RBRACE of Region.t    (* "}" *)

  (* Separators *)

| COMMA of Region.t     (* "," *)
| SEMI  of Region.t     (* ";" *)
| VBAR of Region.t      (* "|" *)
| COLON of Region.t     (* ":" *)
| DOT of Region.t       (* "." *)

  (* Wildcard *)

| WILD of Region.t      (* "_" *)

  (* Comparisons *)

| EQ of Region.t        (* "="  *)
| NE of Region.t        (* "<>" *)
| LT of Region.t        (* "<"  *)
| GT of Region.t        (* ">"  *)
| LE of Region.t        (* "=<" *)
| GE of Region.t        (* ">=" *)

| BOOL_OR of Region.t   (* "||" *)
| BOOL_AND of Region.t  (* "&&" *)

  (* Identifiers, labels, numbers and strings *)

| Ident  of string Region.reg
| Constr of string Region.reg
| Int    of (string * Z.t) Region.reg
| Nat    of (string * Z.t) Region.reg
| Mtz    of (string * Z.t) Region.reg
| Str    of string Region.reg
| Bytes  of (string * Hex.t) Region.reg

  (* Keywords *)

(*| And*)
| Begin of Region.t
| Else of Region.t
| End of Region.t
| False of Region.t
| Fun of Region.t
| If of Region.t
| In of Region.t
| Let of Region.t
| List of Region.t
| Map of Region.t
| Match of Region.t
| Mod of Region.t
| Not of Region.t
| Of of Region.t
| Or of Region.t
| Set of Region.t
| Then of Region.t
| True of Region.t
| Type of Region.t
| With of Region.t

  (* Liquidity specific *)

| LetEntry of Region.t
| MatchNat of Region.t
(*
| Contract
| Sig
| Struct
*)

(* Virtual tokens *)

| EOF of Region.t (* End of file *)

type token = t

let proj_token = function
  | ARROW region -> region, "ARROW"
  | CONS region -> region, "CONS"
  | CAT region -> region, "CAT"
  | MINUS region -> region, "MINUS"
  | PLUS region -> region, "PLUS"
  | SLASH region -> region, "SLASH"
  | TIMES region -> region, "TIMES"
  | LPAR region -> region, "LPAR"
  | RPAR region -> region, "RPAR"
  | LBRACKET region -> region, "LBRACKET"
  | RBRACKET region -> region, "RBRACKET"
  | LBRACE region -> region, "LBRACE"
  | RBRACE region -> region, "RBRACE"
  | COMMA region -> region, "COMMA"
  | SEMI region -> region, "SEMI"
  | VBAR region -> region, "VBAR"
  | COLON region -> region, "COLON"
  | DOT region -> region, "DOT"
  | WILD region -> region, "WILD"
  | EQ region -> region, "EQ"
  | NE region -> region, "NE"
  | LT region -> region, "LT"
  | GT region -> region, "GT"
  | LE region -> region, "LE"
  | GE region -> region, "GE"
  | BOOL_OR region -> region, "BOOL_OR"
  | BOOL_AND region -> region, "BOOL_AND"
  | Ident Region.{region; value} ->
    region, sprintf "Ident %s" value
  | Constr Region.{region; value} ->
    region, sprintf "Constr %s" value
  | Int Region.{region; value = s,n} ->
    region, sprintf "Int (\"%s\", %s)" s (Z.to_string n)
  | Nat Region.{region; value = s,n} ->
    region, sprintf "Nat (\"%s\", %s)" s (Z.to_string n)
  | Mtz Region.{region; value = s,n} ->
    region, sprintf "Mtz (\"%s\", %s)" s (Z.to_string n)
  | Str Region.{region; value} ->
    region, sprintf "Str %s" value
  | Bytes Region.{region; value = s,b} ->
    region,
    sprintf "Bytes (\"%s\", \"0x%s\")"
      s (Hex.to_string b)
  | Begin region -> region, "Begin"
  | Else region -> region, "Else"
  | End region -> region, "End"
  | False region -> region, "False"
  | Fun region -> region, "Fun"
  | If region -> region, "If"
  | In region -> region, "In"
  | Let region -> region, "Let"
  | List region -> region, "List"
  | Map region -> region, "Map"
  | Match region -> region, "Match"
  | Mod region -> region, "Mod"
  | Not region -> region, "Not"
  | Of region -> region, "Of"
  | Or region -> region, "Or"
  | Set region -> region, "Set"
  | Then region -> region, "Then"
  | True region -> region, "True"
  | Type region -> region, "Type"
  | With region -> region, "With"
  | LetEntry region -> region, "LetEntry"
  | MatchNat region -> region, "MatchNat"
  | EOF region -> region, "EOF"

let to_lexeme = function
  | ARROW _ -> "->"
  | CONS _ -> "::"
  | CAT _ -> "^"
  | MINUS _ -> "-"
  | PLUS _ -> "+"
  | SLASH _ -> "/"
  | TIMES _ -> "*"
  | LPAR _ -> "("
  | RPAR _ -> ")"
  | LBRACKET _ -> "["
  | RBRACKET _ -> "]"
  | LBRACE _ -> "{"
  | RBRACE _ -> "}"
  | COMMA _ -> ","
  | SEMI _ -> ";"
  | VBAR _ -> "|"
  | COLON _ -> ":"
  | DOT _ -> "."
  | WILD _ -> "_"
  | EQ _ -> "="
  | NE _ -> "<>"
  | LT _ -> "<"
  | GT _ -> ">"
  | LE _ -> "=<"
  | GE _ -> ">="
  | BOOL_OR _ -> "||"
  | BOOL_AND _ -> "&&"
  | Ident id -> id.Region.value
  | Constr id -> id.Region.value
  | Int i 
  | Nat i 
  | Mtz i -> fst i.Region.value
  | Str s -> s.Region.value
  | Bytes b -> fst b.Region.value
  | Begin _ -> "begin"
  | Else _ -> "else"
  | End _ -> "end"
  | False _ -> "false"
  | Fun _ -> "fun"
  | If _ -> "if"
  | In _ -> "in"
  | Let _ -> "let"
  | List _ -> "list"
  | Map _ -> "map"
  | Match _ -> "match"
  | Mod _ -> "mod"
  | Not _ -> "not"
  | Of _ -> "of"
  | Or _ -> "or"
  | Set _ -> "set"
  | True _ -> "true"
  | Type _ -> "type"
  | Then _ -> "then"
  | With _ -> "with"
  | LetEntry _ -> "let%entry"
  | MatchNat _ -> "match%nat"
  | EOF _ -> ""

let to_string token ?(offsets=true) mode =
  let region, val_str = proj_token token in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str val_str

let to_region token = proj_token token |> fst

(* Injections *)

type int_err =
  Non_canonical_zero

(* LEXIS *)

let keywords = [
  (fun reg -> Begin reg);
  (fun reg -> Else  reg);
  (fun reg -> End   reg);
  (fun reg -> False reg);
  (fun reg -> Fun   reg);
  (fun reg -> If    reg);
  (fun reg -> In    reg);
  (fun reg -> Let   reg);
  (fun reg -> List  reg);
  (fun reg -> Map   reg);
  (fun reg -> Match reg);
  (fun reg -> Mod   reg);
  (fun reg -> Not   reg);
  (fun reg -> Of    reg);
  (fun reg -> Or    reg);
  (fun reg -> Set   reg);
  (fun reg -> Then  reg);
  (fun reg -> True  reg);
  (fun reg -> Type  reg);
  (fun reg -> With  reg);
  (fun reg -> LetEntry reg);
  (fun reg -> MatchNat reg);
]

let reserved =
  let open SSet in
  empty 
    |> add "and"
    |> add "as"
    |> add "asr"
    |> add "class"
    |> add "constraint"
    |> add "do"
    |> add "done"
    |> add "downto"
    |> add "exception"
    |> add "external"
    |> add "for"
    |> add "function"
    |> add "functor"
    |> add "inherit"
    |> add "initializer"
    |> add "land"
    |> add "lazy"
    |> add "lor"
    |> add "lsl"
    |> add "lsr"    
    |> add "lxor"
    |> add "method"
    |> add "module"
    |> add "mutable"
    |> add "new"
    |> add "nonrec"
    |> add "object"
    |> add "open"
    |> add "private"
    |> add "rec"
    |> add "sig"
    |> add "struct"
    |> add "to"
    |> add "try"
    |> add "val"
    |> add "virtual"
    |> add "when"
    |> add "while"

let constructors = [
  (fun reg -> False reg);
  (fun reg -> True  reg);  
]

let add map (key, value) = SMap.add key value map

let mk_map mk_key list =
  let apply map value = add map (mk_key value, value)
  in List.fold_left apply SMap.empty list

type lexis = {
  kwd  : (Region.t -> token) SMap.t;
  cstr : (Region.t -> token) SMap.t;
  res  : SSet.t
}

let lexicon : lexis =
  let build list = mk_map (fun f -> to_lexeme (f Region.ghost)) list
  in {kwd  = build keywords;
      cstr = build constructors;
      res  = reserved}

type ident_err = Reserved_name

}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

let small   = ['a'-'z']
let capital = ['A'-'Z']
let letter  = small | capital
let digit   = ['0'-'9']
let ident   = small (letter | '_' | digit | '%')*
let constr  = capital (letter | '_' | digit)*

(* Rules *)

rule scan_ident region lexicon = parse
  (ident as value) eof {
    if   SSet.mem value lexicon.res
    then Error Reserved_name
    else Ok (match SMap.find_opt value lexicon.kwd with
               Some mk_kwd -> mk_kwd region
             |        None -> Ident Region.{region; value}) }

and scan_constr region lexicon = parse
  (constr as value) eof {
    match SMap.find_opt value lexicon.cstr with
      Some mk_cstr -> mk_cstr region
    |         None -> Constr Region.{region; value} }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

(* Smart constructors (injections) *)

let mk_string lexeme region = Str Region.{region; value=lexeme}

let mk_bytes lexeme region =
  let norm = Str.(global_replace (regexp "_") "" lexeme) in
  let value = lexeme, Hex.of_string norm
  in Bytes Region.{region; value}

let mk_int lexeme region =
  let z = Str.(global_replace (regexp "_") "" lexeme)
          |> Z.of_string in
  if   Z.equal z Z.zero && lexeme <> "0"
  then Error Non_canonical_zero
  else Ok (Int Region.{region; value = lexeme, z})

let mk_nat lexeme region =
  let z =
    Str.(global_replace (regexp "_") "" lexeme) |>
    Str.(global_replace (regexp "n") "") |>
    Z.of_string in
  if Z.equal z Z.zero && lexeme <> "0n"
  then Error Non_canonical_zero
  else Ok (Nat Region.{region; value = lexeme, z})

let mk_mtz lexeme region =
  let z =
    Str.(global_replace (regexp "_") "" lexeme) |>
    Str.(global_replace (regexp "mtz") "") |>
    Z.of_string in
  if Z.equal z Z.zero && lexeme <> "0mtz"
  then Error Non_canonical_zero
  else Ok (Mtz Region.{region; value = lexeme, z})

let eof region = EOF region

let mk_sym lexeme region =
  match lexeme with
    "->"   ->   ARROW     region
  | "::"   ->   CONS      region
  | "^"   ->    CAT       region
  | "-"   ->    MINUS     region
  | "+"   ->    PLUS      region
  | "/"   ->    SLASH     region
  | "*"   ->    TIMES     region
  | "["   ->    LBRACKET  region
  | "]"   ->    RBRACKET  region
  | "{"   ->    LBRACE    region
  | "}"  ->     RBRACE    region
  | ","  ->     COMMA     region
  | ";"   ->    SEMI      region
  | "|"   ->    VBAR      region
  | ":"   ->    COLON     region
  | "."  ->     DOT       region
  | "_"   ->    WILD      region
  | "="  ->     EQ        region
  | "<>" ->     NE        region
  | "<"   ->    LT        region
  | ">"   ->    GT        region
  | "=<"   ->   LE        region
  | ">="   ->   GE        region
  | "||"   ->   BOOL_OR   region
  | "&&"   ->   BOOL_AND  region
  | "("    ->   LPAR      region
  | ")"    ->   RPAR      region
  |  a  ->   failwith ("Not understood token: " ^ a)

(* Identifiers *)

let mk_ident' lexeme region lexicon =
  Lexing.from_string lexeme |> scan_ident region lexicon

let mk_ident lexeme region = mk_ident' lexeme region lexicon

(* Constructors *)

let mk_constr' lexeme region lexicon =
  Lexing.from_string lexeme |> scan_constr region lexicon

let mk_constr lexeme region = mk_constr' lexeme region lexicon

(* Predicates *)

let is_string = function
  Str _ -> true
|        _ -> false

let is_bytes = function
  Bytes _ -> true
|       _ -> false

let is_int = function
  Int _ -> true
|     _ -> false

let is_ident = function
  Ident _ -> true
|       _ -> false

let is_kwd = function
  | Begin _
  | Else _
  | End _
  | False _
  | Fun _
  | If _
  | In _
  | Let _
  | List _
  | Map _
  | Match _
  | Mod _
  | Not _
  | Of _
  | Or _
  | Set _
  | Then _
  | True _
  | Type _
  | LetEntry _
  | MatchNat _
  | With _ -> true
  | _ -> false

let is_constr = function
| Constr  _
| Ident _
| False _
| True _    -> true
| _         -> false

let is_sym = function
| ARROW _
| CONS _
| CAT _
| MINUS _
| PLUS _
| SLASH _
| TIMES _
| LPAR _
| RPAR _
| LBRACKET _
| RBRACKET _
| LBRACE _
| RBRACE _
| COMMA _
| SEMI  _
| VBAR _
| COLON _
| DOT _
| WILD _
| EQ _
| NE _
| LT _
| GT _
| LE _
| GE _
| BOOL_OR _
| BOOL_AND _ -> true
|          _ -> false

let is_eof = function EOF _ -> true | _ -> false

(* END TRAILER *)
}