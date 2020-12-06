(* ocamlex specification for ReasonLIGO *)
{
(* START HEADER *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Markup = LexerLib.Markup

(* Utility modules *)

module SMap = Map.Make (String)
module SSet = Set.Make (String)

(* TOKENS *)

type lexeme = string

module T =
  struct
    type t =
      (* Literals *)

      String   of lexeme Region.reg
    | Verbatim of lexeme Region.reg
    | Bytes    of (lexeme * Hex.t) Region.reg
    | Int      of (lexeme * Z.t) Region.reg
    | Nat      of (lexeme * Z.t) Region.reg
    | Mutez    of (lexeme * Z.t) Region.reg
    | Ident    of lexeme Region.reg
    | Constr   of lexeme Region.reg
    | Lang     of lexeme Region.reg Region.reg
    | Attr     of string Region.reg

    (* Symbols *)

    | MINUS    of Region.t  (* "-"    *)
    | PLUS     of Region.t  (* "+"    *)
    | SLASH    of Region.t  (* "/"    *)
    | TIMES    of Region.t  (* "*"    *)
    | REM      of Region.t  (* "%"    *)
    | PLUS2    of Region.t  (* "++"   *)
    | MINUS2   of Region.t  (* "--"   *)

    | LPAR     of Region.t  (* "("    *)
    | RPAR     of Region.t  (* ")"    *)
    | LBRACKET of Region.t  (* "["    *)
    | RBRACKET of Region.t  (* "]"    *)
    | LBRACE   of Region.t  (* "{"    *)
    | RBRACE   of Region.t  (* "}"    *)

    | COMMA    of Region.t  (* ","    *)
    | SEMI     of Region.t  (* ";"    *)
    | COLON    of Region.t  (* ":"    *)
    | DOT      of Region.t  (* "."    *)
    | ELLIPSIS of Region.t  (* "..."  *)
    | QMARK    of Region.t  (* "?"    *)

    | BOOL_OR  of Region.t  (* "||"   *)
    | BOOL_AND of Region.t  (* "&&"   *)
    | BOOL_NOT of Region.t  (* "!"    *)

    | BIT_OR   of Region.t  (* "|"    *)
    | BIT_AND  of Region.t  (* "&"    *)
    | BIT_NOT  of Region.t  (* "~"    *)
    | BIT_XOR  of Region.t  (* "^"    *)
    | SHIFT_L  of Region.t  (* "<<<"  *)
    | SHIFT_R  of Region.t  (* ">>>"  *)

    | EQ       of Region.t  (* "="    *)
    | EQ3      of Region.t  (* "==="  *)
    | NE2      of Region.t  (* "!=="  *)

    | LT       of Region.t  (* "<"    *)
    | GT       of Region.t  (* ">"    *)
    | LE       of Region.t  (* "<="   *)
    | GE       of Region.t  (* ">="   *)

    | PLUS_EQ  of Region.t  (* "+="   *)
    | MINUS_EQ of Region.t  (* "-="   *)
    | MULT_EQ  of Region.t  (* "*="   *)
    | REM_EQ   of Region.t  (* "%="   *)
    | DIV_EQ   of Region.t  (* "/="   *)
    | SL_EQ    of Region.t  (* "<<<=" *)
    | SR_EQ    of Region.t  (* ">>>=" *)
    | AND_EQ   of Region.t  (* "&="   *)
    | OR_EQ    of Region.t  (* "|="   *)
    | XOR_EQ   of Region.t  (* "^="   *)

    | VBAR     of Region.t  (* "|"    *)
    | ARROW    of Region.t  (* "=>"   *)
    | WILD     of Region.t  (* "_"    *)

    (* JavaScript Keywords *)

    | Break    of Region.t  (* break    *)
    | Case     of Region.t  (* case     *)
    | Class    of Region.t  (* class    *)
    | Const    of Region.t  (* const    *)
    | Default  of Region.t  (* default  *)
    | Else     of Region.t  (* else     *)
    | Enum     of Region.t  (* enum     *)
    | False    of Region.t  (* false    *)
    | For      of Region.t  (* for      *)
    | If       of Region.t  (* if       *)
    | Let      of Region.t  (* let      *)
    | New      of Region.t  (* new      *)
    | Return   of Region.t  (* return   *)
    | Switch   of Region.t  (* switch   *)
    | This     of Region.t  (* this     *)
    | True     of Region.t  (* true     *)
    | Void     of Region.t  (* void     *)
    | While    of Region.t  (* while    *)
    | With     of Region.t  (* with     *)

    (* TypeScript keywords *)

    | Type        of Region.t  (* type        *)
    | Constructor of Region.t  (* constructor *)
    | Get         of Region.t  (* get         *)
    | Set         of Region.t  (* set         *)

    (* Data constructors *)

    | C_None  of Region.t  (* None *)
    | C_Some  of Region.t  (* Some *)

    (* Virtual tokens *)

    | EOF of Region.t

    (* Unlexing the tokens *)

    let gen_sym prefix =
      let count = ref 0 in
      fun () -> incr count;
             prefix ^ string_of_int !count

    let id_sym   = gen_sym "id"
    and ctor_sym = gen_sym "C"

    let concrete = function
        (* Identifiers, labels, numbers and strings *)

      "Ident"    -> id_sym ()
    | "Constr"   -> ctor_sym ()
    | "Int"      -> "1"
    | "Nat"      -> "1n"
    | "Mutez"    -> "1mutez"
    | "String"   -> "\"a string\""
    | "Verbatim" -> "{|verbatim|}"
    | "Bytes"    -> "0xAA"
    | "Attr"     -> "[@attr]"
    | "Lang"     -> "[%Michelson"

    (* Symbols *)

    | "MINUS"    -> "-"
    | "PLUS"     -> "+"
    | "SLASH"    -> "/"
    | "TIMES"    -> "*"
    | "REM"      -> "%"
    | "PLUS2"    -> "++"
    | "MINUS2"   -> "--"

    | "LPAR"     -> "("
    | "RPAR"     -> ")"
    | "LBRACKET" -> "["
    | "RBRACKET" -> "]"
    | "LBRACE"   -> "{"
    | "RBRACE"   -> "}"

    | "COMMA"    -> ","
    | "SEMI"     -> ";"
    | "COLON"    -> ":"
    | "DOT"      -> "."
    | "ELLIPSIS" -> "..."
    | "QMARK"    -> "?"

    | "BOOL_OR"  -> "||"
    | "BOOL_AND" -> "&&"
    | "BOOL_NOT" -> "!"

    | "BIT_OR"   -> "|"
    | "BIT_AND"  -> "&"
    | "BIT_NOT"  -> "~"
    | "BIT_XOR"  -> "^"
    | "SHIFT_L"  -> "<<<"
    | "SHIFT_R"  -> ">>>"

    | "EQ"       -> "="
    | "EQ3"      -> "==="
    | "NE2"      -> "!=="

    | "LT"       -> "<"
    | "GT"       -> ">"
    | "LE"       -> "<="
    | "GE"       -> ">="

    | "PLUS_EQ"  -> "+="
    | "MINUS_EQ" -> "-="
    | "MULT_EQ"  -> "*="
    | "REM_EQ"   -> "%="
    | "DIV_EQ"   -> "/="
    | "SL_EQ"    -> "<<<="
    | "SR_EQ"    -> ">>>="
    | "AND_EQ"   -> "&="
    | "OR_EQ"    -> "|="
    | "XOR_EQ"   -> "^="

    | "VBAR"     -> "|"
    | "ARROW"    -> "=>"
    | "WILD"     -> "_"

    (* JavaScript Keywords *)

    | "Break"    -> "break"
    | "Case"     -> "case"
    | "Class"    -> "class"
    | "Const"    -> "const"
    | "Default"  -> "default"
    | "Else"     -> "else"
    | "Enum"     -> "enum"
    | "False"    -> "false"
    | "For"      -> "for"
    | "If"       -> "if"
    | "Let"      -> "let"
    | "New"      -> "new"
    | "Return"   -> "return"
    | "Switch"   -> "switch"
    | "This"     -> "this"
    | "True"     -> "true"
    | "Void"     -> "void"
    | "While"    -> "while"
    | "With"     -> "with"

    (* TypeScript keywords *)

    | "Type"        -> "type"
    | "Constructor" -> "constructor"
    | "Get"         -> "get"
    | "Set"         -> "set"

    (* Data constructors *)

    | "C_None" -> "None"
    | "C_Some" -> "Some"

    (* Virtual tokens *)

    | "EOF" -> ""

    (* This case should not happen! *)

    | _  -> "\\Unknown" (* Backslash meant to trigger an error *)

    (* Projections *)

    let sprintf = Printf.sprintf

    type token = t

    let proj_token = function
      (* Literals *)

      String Region.{region; value} ->
        region, sprintf "String %S" value
    | Verbatim Region.{region; value} ->
        region, sprintf "Verbatim %S" value
    | Bytes Region.{region; value = s,b} ->
        region,
        sprintf "Bytes (%S, \"0x%s\")" s (Hex.show b)
    | Int Region.{region; value = s,n} ->
        region, sprintf "Int (%S, %s)" s (Z.to_string n)
    | Nat Region.{region; value = s,n} ->
        region, sprintf "Nat (%S, %s)" s (Z.to_string n)
    | Mutez Region.{region; value = s,n} ->
        region, sprintf "Mutez (%S, %s)" s (Z.to_string n)
    | Ident Region.{region; value} ->
        region, sprintf "Ident %S" value
    | Constr Region.{region; value} ->
        region, sprintf "Constr %S" value
    | Lang Region.{region; value} ->
        region, sprintf "Lang %S" (value.Region.value)
    | Attr Region.{region; value} ->
        region, sprintf "Attr %S" value

    (* Symbols *)

    | MINUS    region -> region, "MINUS"
    | PLUS     region -> region, "PLUS"
    | SLASH    region -> region, "SLASH"
    | TIMES    region -> region, "TIMES"
    | REM      region -> region, "REM"
    | PLUS2    region -> region, "PLUS2"
    | MINUS2   region -> region, "MINUS2"

    | LPAR     region -> region, "LPAR"
    | RPAR     region -> region, "RPAR"
    | LBRACKET region -> region, "LBRACKET"
    | RBRACKET region -> region, "RBRACKET"
    | LBRACE   region -> region, "LBRACE"
    | RBRACE   region -> region, "RBRACE"

    | COMMA    region -> region, "COMMA"
    | SEMI     region -> region, "SEMI"
    | COLON    region -> region, "COLON"
    | DOT      region -> region, "DOT"
    | ELLIPSIS region -> region, "ELLIPSIS"
    | QMARK    region -> region, "QMARK"

    | BOOL_OR  region -> region, "BOOL_OR"
    | BOOL_AND region -> region, "BOOL_AND"
    | BOOL_NOT region -> region, "BOOL_NOT"

    | BIT_OR   region -> region, "BIT_OR"
    | BIT_AND  region -> region, "BIT_AND"
    | BIT_NOT  region -> region, "BIT_NOT"
    | BIT_XOR  region -> region, "BIT_XOR"
    | SHIFT_L  region -> region, "SHIFT_L"
    | SHIFT_R  region -> region, "SHIFT_R"

    | EQ       region -> region, "EQ"
    | EQ3      region -> region, "EQ3"
    | NE2      region -> region, "NE2"

    | LT       region -> region, "LT"
    | GT       region -> region, "GT"
    | LE       region -> region, "LE"
    | GE       region -> region, "GE"

    | PLUS_EQ  region -> region, "PLUS_EQ"
    | MINUS_EQ region -> region, "MINUS_EQ"
    | MULT_EQ  region -> region, "MULT_EQ"
    | REM_EQ   region -> region, "REM_EQ"
    | DIV_EQ   region -> region, "DIV_EQ"
    | SL_EQ    region -> region, "SL_EQ"
    | SR_EQ    region -> region, "SR_EQ"
    | AND_EQ   region -> region, "AND_EQ"
    | OR_EQ    region -> region, "OR_EQ"
    | XOR_EQ   region -> region, "XOR_EQ"

    | VBAR     region -> region, "VBAR"
    | ARROW    region -> region, "ARROW"
    | WILD     region -> region, "WILD"

    (* JavaScript Keywords *)

    | Break    region -> region, "Break"
    | Case     region -> region, "Case"
    | Class    region -> region, "Class"
    | Const    region -> region, "Const"
    | Default  region -> region, "Default"
    | Else     region -> region, "Else"
    | Enum     region -> region, "Enum"
    | False    region -> region, "False"
    | For      region -> region, "For"
    | If       region -> region, "If"
    | Let      region -> region, "Let"
    | New      region -> region, "New"
    | Return   region -> region, "Return"
    | Switch   region -> region, "Switch"
    | This     region -> region, "This"
    | True     region -> region, "True"
    | Void     region -> region, "Void"
    | While    region -> region, "While"
    | With     region -> region, "With"

    (* TypeScript keywords *)

    | Type        region -> region, "Type"
    | Constructor region -> region, "Constructor"
    | Get         region -> region, "Get"
    | Set         region -> region, "Set"

    (* Data constructors *)

    | C_None region -> region, "C_None"
    | C_Some region -> region, "C_Some"
    | EOF    region -> region, "EOF"


    let to_lexeme = function
      (* Literals *)

      String s   -> sprintf "%S" (String.escaped s.Region.value)
    | Verbatim v -> String.escaped v.Region.value
    | Bytes b    -> fst b.Region.value
    | Int i
    | Nat i
    | Mutez i    -> fst i.Region.value
    | Ident id   -> id.Region.value
    | Constr id  -> id.Region.value
    | Attr a     -> sprintf "[@%s]" a.Region.value
    | Lang lang  -> Region.(lang.value.value)

    (* Symbols *)

    | MINUS    _ -> "-"
    | PLUS     _ -> "+"
    | SLASH    _ -> "/"
    | TIMES    _ -> "*"
    | REM      _ -> "%"
    | PLUS2    _ -> "++"
    | MINUS2   _ -> "--"

    | LPAR     _ -> "("
    | RPAR     _ -> ")"
    | LBRACKET _ -> "["
    | RBRACKET _ -> "]"
    | LBRACE   _ -> "{"
    | RBRACE   _ -> "}"

    | COMMA    _ -> ","
    | SEMI     _ -> ";"
    | COLON    _ -> ":"
    | DOT      _ -> "."
    | ELLIPSIS _ -> "..."
    | QMARK    _ -> "?"

    | BOOL_OR  _ -> "||"
    | BOOL_AND _ -> "&&"
    | BOOL_NOT _ -> "!"

    | BIT_OR   _ -> "|"
    | BIT_AND  _ -> "&"
    | BIT_NOT  _ -> "~"
    | BIT_XOR  _ -> "^"
    | SHIFT_L  _ -> "<<<"
    | SHIFT_R  _ -> ">>>"

    | EQ       _ -> "="
    | EQ3      _ -> "==="
    | NE2      _ -> "!=="

    | LT       _ -> "<"
    | GT       _ -> ">"
    | LE       _ -> "<="
    | GE       _ -> ">="

    | PLUS_EQ  _ -> "+="
    | MINUS_EQ _ -> "-="
    | MULT_EQ  _ -> "*="
    | REM_EQ   _ -> "%="
    | DIV_EQ   _ -> "/="
    | SL_EQ    _ -> "<<<="
    | SR_EQ    _ -> ">>>="
    | AND_EQ   _ -> "&="
    | OR_EQ    _ -> "|="
    | XOR_EQ   _ -> "^="

    | VBAR     _ -> "|"
    | ARROW    _ -> "=>"
    | WILD     _ -> "_"

    (* JavaScript Keywords *)

    | Break    _ -> "break"
    | Case     _ -> "case"
    | Class    _ -> "class"
    | Const    _ -> "const"
    | Default  _ -> "default"
    | Else     _ -> "else"
    | Enum     _ -> "enum"
    | False    _ -> "false"
    | For      _ -> "for"
    | If       _ -> "if"
    | Let      _ -> "let"
    | New      _ -> "new"
    | Return   _ -> "return"
    | Switch   _ -> "switch"
    | This     _ -> "this"
    | True     _ -> "true"
    | Void     _ -> "void"
    | While    _ -> "while"
    | With     _ -> "with"

    (* TypeScript keywords *)

    | Type        _ -> "type"
    | Constructor _ -> "constructor"
    | Get         _ -> "get"
    | Set         _ -> "set"

    (* Data constructors *)

    | C_None  _ -> "None"
    | C_Some  _ -> "Some"

    (* Virtual tokens *)

    | EOF _ -> ""

    (* CONVERSIONS *)

    let to_string ~offsets mode token =
      let region, val_str = proj_token token in
      let reg_str = region#compact ~offsets mode
      in sprintf "%s: %s" reg_str val_str

    let to_region token = proj_token token |> fst

    (* LEXIS *)

    let keywords = [
        (* JavaScript Keywords *)

       (fun reg -> Break   reg);
       (fun reg -> Case    reg);
       (fun reg -> Class   reg);
       (fun reg -> Const   reg);
       (fun reg -> Default reg);
       (fun reg -> Else    reg);
       (fun reg -> Enum    reg);
       (fun reg -> False   reg);
       (fun reg -> For     reg);
       (fun reg -> If      reg);
       (fun reg -> Let     reg);
       (fun reg -> New     reg);
       (fun reg -> Return  reg);
       (fun reg -> Switch  reg);
       (fun reg -> This    reg);
       (fun reg -> True    reg);
       (fun reg -> Void    reg);
       (fun reg -> While   reg);
       (fun reg -> With    reg);

       (* TypeScript keywords *)

       (fun reg -> Type        reg);
       (fun reg -> Constructor reg);
       (fun reg -> Get         reg);
       (fun reg -> Set         reg)
    ]

    let reserved = SSet.empty

    let constructors = [
      (fun reg -> C_None reg);
      (fun reg -> C_Some reg)
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
      let build = mk_map (fun f -> to_lexeme (f Region.ghost))
      in {kwd  = build keywords;
          cstr = build constructors;
          res  = reserved}

    (* Keywords *)

    type kwd_err = Invalid_keyword

    let mk_kwd ident region =
      match SMap.find_opt ident lexicon.kwd with
        Some mk_kwd -> Ok (mk_kwd region)
      |        None -> Error Invalid_keyword

    (* Identifiers *)

    type ident_err = Reserved_name

(* END OF HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

let small   = ['a'-'z']
let capital = ['A'-'Z']
let letter  = small | capital
let digit   = ['0'-'9']
let ident   = small (letter | '_' | digit)*
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

  let mk_string lexeme region =
    String Region.{region; value=lexeme}

  let mk_verbatim lexeme region =
    Verbatim Region.{region; value=lexeme}

  let mk_bytes lexeme region =
    let norm = Str.(global_replace (regexp "_") "" lexeme) in
    let value = lexeme, `Hex norm
    in Bytes Region.{region; value}

  type int_err = Non_canonical_zero

  let mk_int lexeme region =
    let z =
      Str.(global_replace (regexp "_") "" lexeme) |> Z.of_string
    in if   Z.equal z Z.zero && lexeme <> "0"
       then Error Non_canonical_zero
       else Ok (Int Region.{region; value = lexeme,z})

  type nat_err =
    Invalid_natural
  | Non_canonical_zero_nat

  let mk_nat lexeme region =
    match String.index_opt lexeme 'n' with
      None -> Error Invalid_natural
    | Some _ ->
        let z =
          Str.(global_replace (regexp "_") "" lexeme) |>
            Str.(global_replace (regexp "n") "") |>
            Z.of_string in
        if   Z.equal z Z.zero && lexeme <> "0n"
        then Error Non_canonical_zero_nat
        else Ok (Nat Region.{region; value = lexeme,z})

  let mk_mutez lexeme region =
    let z = Str.(global_replace (regexp "_") "" lexeme) |>
              Str.(global_replace (regexp "mutez") "") |>
              Z.of_string in
    if   Z.equal z Z.zero && lexeme <> "0mutez"
    then Error Non_canonical_zero
    else Ok (Mutez Region.{region; value = lexeme, z})

  let eof region = EOF region

  type sym_err = Invalid_symbol

  let mk_sym lexeme region =
    match lexeme with
      (* Lexemes in common with all concrete syntaxes *)

      ";"   -> Ok (SEMI     region)
    | ","   -> Ok (COMMA    region)
    | "("   -> Ok (LPAR     region)
    | ")"   -> Ok (RPAR     region)
    | "["   -> Ok (LBRACKET region)
    | "]"   -> Ok (RBRACKET region)
    | "{"   -> Ok (LBRACE   region)
    | "}"   -> Ok (RBRACE   region)
    | "="   -> Ok (EQ       region)
    | ":"   -> Ok (COLON    region)
    | "|"   -> Ok (VBAR     region)
    | "."   -> Ok (DOT      region)
    | "_"   -> Ok (WILD     region)
    | "+"   -> Ok (PLUS     region)
    | "-"   -> Ok (MINUS    region)
    | "*"   -> Ok (TIMES    region)
    | "/"   -> Ok (SLASH    region)
    | "<"   -> Ok (LT       region)
    | "<="  -> Ok (LE       region)
    | ">"   -> Ok (GT       region)
    | ">="  -> Ok (GE       region)

    (* Symbols specific to JsLIGO *)

    | "%"   -> Ok (REM      region)
    | "++"  -> Ok (PLUS2    region)
    | "--"  -> Ok (MINUS2   region)

    | "..." -> Ok (ELLIPSIS region)
    |  "?"  -> Ok (QMARK    region)

    | "||"  -> Ok (BOOL_OR  region)
    | "&&"  -> Ok (BOOL_AND region)
    | "!"   -> Ok (BOOL_NOT region)

    | "&"   -> Ok (BIT_AND  region)
    | "~"   -> Ok (BIT_NOT  region)
    | "^"   -> Ok (BIT_XOR  region)
    | "<<<" -> Ok (SHIFT_L  region)
    | ">>>" -> Ok (SHIFT_R  region)

    | "===" -> Ok (EQ3      region)
    | "!==" -> Ok (NE2      region)

    | "+="  -> Ok (PLUS_EQ  region)
    | "-="  -> Ok (MINUS_EQ region)
    | "*="  -> Ok (MULT_EQ  region)
    | "%="  -> Ok (REM_EQ   region)

    | "/="   -> Ok (DIV_EQ  region)
    | "<<<=" -> Ok (SL_EQ   region)
    | ">>>=" -> Ok (SR_EQ   region)
    | "&="   -> Ok (AND_EQ  region)
    | "|="   -> Ok (OR_EQ   region)
    | "^="   -> Ok (XOR_EQ  region)

    | "=>"   -> Ok (ARROW   region)

      (* Invalid symbols *)

    | _ ->  Error Invalid_symbol


    (* Identifiers *)

    let mk_ident lexeme region =
      Lexing.from_string lexeme |> scan_ident region lexicon

    (* Constructors *)

    let mk_constr lexeme region =
      Lexing.from_string lexeme |> scan_constr region lexicon

    (* Attributes *)

    let mk_attr lexeme region = Attr Region.{value=lexeme; region}

    (* Code injection *)

    let mk_lang lang region = Lang Region.{value=lang; region}

    (* Predicates *)

    let is_string   = function String _   -> true | _ -> false
    let is_verbatim = function Verbatim _ -> true | _ -> false
    let is_bytes    = function Bytes _    -> true | _ -> false
    let is_int      = function Int _      -> true | _ -> false
    let is_nat      = function Nat _      -> true | _ -> false
    let is_mutez    = function Mutez _    -> true | _ -> false
    let is_ident    = function Ident _    -> true | _ -> false
    let is_constr   = function Constr _   -> true | _ -> false
    let is_lang     = function Lang _     -> true | _ -> false
    let is_minus    = function MINUS _    -> true | _ -> false
    let is_eof      = function EOF _      -> true | _ -> false

    let is_hexa = function
      Constr Region.{value="A"|"a"|"B"|"b"|"C"|"c"
                     |"D"|"d"|"E"|"e"|"F"|"f"; _} -> true
    | _ -> false

    let is_sym = function
      MINUS _  (* "-"    *)
    | PLUS     _ (* "+"    *)
    | SLASH    _ (* "/"    *)
    | TIMES    _ (* "*"    *)
    | REM      _ (* "%"    *)
    | PLUS2    _ (* "++"   *)
    | MINUS2   _ (* "--"   *)

    | LPAR     _ (* "("    *)
    | RPAR     _ (* ")"    *)
    | LBRACKET _ (* "["    *)
    | RBRACKET _ (* "]"    *)
    | LBRACE   _ (* "{"    *)
    | RBRACE   _ (* "}"    *)

    | COMMA    _ (* ","    *)
    | SEMI     _ (* ";"    *)
    | COLON    _ (* ":"    *)
    | DOT      _ (* "."    *)
    | ELLIPSIS _ (* "..."  *)
    | QMARK    _ (* "?"    *)

    | BOOL_OR  _ (* "||"   *)
    | BOOL_AND _ (* "&&"   *)
    | BOOL_NOT _ (* "!"    *)

    | BIT_OR   _ (* "|"    *)
    | BIT_AND  _ (* "&"    *)
    | BIT_NOT  _ (* "~"    *)
    | BIT_XOR  _ (* "^"    *)
    | SHIFT_L  _ (* "<<<"  *)
    | SHIFT_R  _ (* ">>>"  *)

    | EQ       _ (* "="    *)
    | EQ3      _ (* "==="  *)
    | NE2      _ (* "!=="  *)

    | LT       _ (* "<"    *)
    | GT       _ (* ">"    *)
    | LE       _ (* "<="   *)
    | GE       _ (* ">="   *)

    | PLUS_EQ  _ (* "+="   *)
    | MINUS_EQ _ (* "-="   *)
    | MULT_EQ  _ (* "*="   *)
    | REM_EQ   _ (* "%="   *)
    | DIV_EQ   _ (* "/="   *)
    | SL_EQ    _ (* "<<<=" *)
    | SR_EQ    _ (* ">>>=" *)
    | AND_EQ   _ (* "&="   *)
    | OR_EQ    _ (* "|="   *)
    | XOR_EQ   _ (* "^="   *)

    | VBAR     _ (* "|"    *)
    | ARROW    _ (* "=>"   *)
    | WILD     _ (* "_"    *) -> true
    | _ -> false

  end

include T

module type S = module type of T

(* END TRAILER *)
}