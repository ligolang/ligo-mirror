(* Token specification for CameLIGO *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Markup    = LexerLib.Markup
module Directive = LexerLib.Directive

(* Utility modules and types *)

module SMap = Map.Make (String)
module Wrap = Lexing_shared.Wrap

type 'a wrap = 'a Wrap.t
type 'a reg  = 'a Region.reg

let wrap = Wrap.wrap

(* TOKENS *)

type lexeme = string

module T =
  struct
    (* Tokens *)

    type t =
      (* Preprocessing directives *)

      Directive of Directive.t

      (* Literals *)

    | String   of lexeme wrap
    | Verbatim of lexeme wrap
    | Bytes    of (lexeme * Hex.t) wrap
    | Int      of (lexeme * Z.t) wrap
    | Nat      of (lexeme * Z.t) wrap
    | Mutez    of (lexeme * Z.t) wrap
    | Ident    of lexeme wrap
    | UIdent   of lexeme wrap
    | Lang     of lexeme reg reg
    | Attr     of string wrap

    (* Symbols *)

    | ARROW    of lexeme wrap  (* "->" *)
    | CONS     of lexeme wrap  (* "::" *)
    | CARET    of lexeme wrap  (* "^"  *)
    | MINUS    of lexeme wrap  (* "-"  *)
    | PLUS     of lexeme wrap  (* "+"  *)
    | SLASH    of lexeme wrap  (* "/"  *)
    | TIMES    of lexeme wrap  (* "*"  *)
    | LPAR     of lexeme wrap  (* "("  *)
    | RPAR     of lexeme wrap  (* ")"  *)
    | LBRACKET of lexeme wrap  (* "["  *)
    | RBRACKET of lexeme wrap  (* "]"  *)
    | LBRACE   of lexeme wrap  (* "{"  *)
    | RBRACE   of lexeme wrap  (* "}"  *)
    | COMMA    of lexeme wrap  (* ","  *)
    | SEMI     of lexeme wrap  (* ";"  *)
    | VBAR     of lexeme wrap  (* "|"  *)
    | COLON    of lexeme wrap  (* ":"  *)
    | DOT      of lexeme wrap  (* "."  *)
    | WILD     of lexeme wrap  (*  "_" *)
    | EQ       of lexeme wrap  (* "="  *)
    | NE       of lexeme wrap  (* "<>" *)
    | LT       of lexeme wrap  (* "<"  *)
    | GT       of lexeme wrap  (* ">"  *)
    | LE       of lexeme wrap  (* "<=" *)
    | GE       of lexeme wrap  (* ">=" *)
    | BOOL_OR  of lexeme wrap  (* "||" *)
    | BOOL_AND of lexeme wrap  (* "&&" *)
    | QUOTE    of lexeme wrap  (* "'"  *)

    (* Keywords *)

    | Begin     of lexeme wrap  (* begin  *)
    | Else      of lexeme wrap  (* else   *)
    | End       of lexeme wrap  (* end    *)
    | Fun       of lexeme wrap  (* fun    *)
    | Rec       of lexeme wrap  (* rec    *)
    | If        of lexeme wrap  (* if     *)
    | In        of lexeme wrap  (* in     *)
    | Let       of lexeme wrap  (* let    *)
    | Match     of lexeme wrap  (* match  *)
    | Mod       of lexeme wrap  (* mod    *)
    | Land      of lexeme wrap  (* land   *)
    | Lor       of lexeme wrap  (* lor    *)
    | Lxor      of lexeme wrap  (* lxor   *)
    | Lsl       of lexeme wrap  (* lsl    *)
    | Lsr       of lexeme wrap  (* lsr    *)
    | Not       of lexeme wrap  (* not    *)
    | Of        of lexeme wrap  (* of     *)
    | Or        of lexeme wrap  (* or     *)
    | Then      of lexeme wrap  (* then   *)
    | Type      of lexeme wrap  (* type   *)
    | With      of lexeme wrap  (* with   *)
    | Module    of lexeme wrap  (* module *)
    | Struct    of lexeme wrap  (* struct *)

    (* End-Of-File *)

    | EOF of lexeme wrap

    (* Unlexing the tokens *)

    let gen_sym prefix =
      let count = ref 0 in
      fun () -> incr count; prefix ^ string_of_int !count

    let id_sym   = gen_sym "id"
    and ctor_sym = gen_sym "C"

    let concrete = function
      (* Literals *)

      "Ident"    -> id_sym ()
    | "UIdent"   -> ctor_sym ()
    | "Int"      -> "1"
    | "Nat"      -> "1n"
    | "Mutez"    -> "1mutez"
    | "String"   -> "\"a string\""
    | "Verbatim" -> "{|verbatim|}"
    | "Bytes"    -> "0xAA"
    | "Attr"     -> "[@attr]"
    | "Lang"     -> "[%Michelson"

    (* Symbols *)

    | "ARROW" ->   "->"
    | "CONS"  ->   "::"
    | "CARET" ->   "^"

    (* Arithmetics *)

    | "MINUS"   -> "-"
    | "PLUS"    -> "+"
    | "SLASH"   -> "/"
    | "TIMES"   -> "*"

    (* Compounds *)

    | "LPAR"     -> "("
    | "RPAR"     -> ")"
    | "LBRACKET" -> "["
    | "RBRACKET" -> "]"
    | "LBRACE"   -> "{"
    | "RBRACE"   -> "}"

    (* Separators *)

    | "COMMA" -> ","
    | "SEMI"  -> ";"
    | "VBAR"  -> "|"
    | "COLON" -> ":"
    | "DOT"   -> "."

    (* Wildcard *)

    | "WILD" -> "_"

    (* Comparisons *)

    | "EQ" -> "="
    | "NE" -> "<>"
    | "LT" -> "<"
    | "GT" -> ">"
    | "LE" -> "<="
    | "GE" -> ">="

    | "BOOL_OR"  -> "||"
    | "BOOL_AND" -> "&&"

    | "QUOTE" -> "'"

    (* Keywords *)

    | "Begin"  -> "begin"
    | "Else"   -> "else"
    | "End"    -> "end"
    | "Fun"    -> "fun"
    | "Rec"    -> "rec"
    | "If"     -> "if"
    | "In"     -> "in"
    | "Let"    -> "let"
    | "Match"  -> "match"
    | "Mod"    -> "mod"
    | "Land"   -> "land"
    | "Lor"    -> "lor"
    | "Lxor"   -> "lxor"
    | "Lsl"    -> "lsl"
    | "Lsr"    -> "lsr"
    | "Not"    -> "not"
    | "Of"     -> "of"
    | "Or"     -> "or"
    | "Then"   -> "then"
    | "Type"   -> "type"
    | "With"   -> "with"
    | "Module" -> "module"
    | "Struct" -> "struct"

    (* End-Of-File *)

    | "EOF" -> ""

    (* This case should not happen! *)

    | _  -> "\\Unknown" (* Backslash meant to trigger an error *)


    (* Projections *)

    let sprintf = Printf.sprintf

    type token = t

    (* Verbatim strings need to be escaped, but OCaml escaping
       function for strings escapes the double quotes, so we need to
       unescape those. *)

    let escape_verbatim s =
      let escaped = String.escaped s in
      let regexp = Str.regexp "\\\\\"" in
      Str.global_replace regexp "\"" escaped

    let proj_token = function
      (* Preprocessing directives *)
      Directive d ->
        Directive.project d

      (* Literals *)

    | String t ->
        t#region, sprintf "String %S" t#payload
    | Verbatim t ->
        t#region, sprintf "Verbatim %S" t#payload
    | Bytes t ->
        let (s,b) = t#payload in
        t#region,
        sprintf "Bytes (%S, \"0x%s\")" s (Hex.show b)
    | Int t ->
        let (s,n) = t#payload in
        t#region, sprintf "Int (%S, %s)" s (Z.to_string n)
    | Nat t ->
        let (s,n) = t#payload in
        t#region, sprintf "Nat (%S, %s)" s (Z.to_string n)
    | Mutez t ->
        let (s,n) = t#payload in
        t#region, sprintf "Mutez (%S, %s)" s (Z.to_string n)
    | Ident t ->
        t#region, sprintf "Ident %S" t#payload
    | UIdent t ->
        t#region, sprintf "UIdent %S" t#payload
    | Lang {value = {value = payload; _}; region; _} ->
        region, sprintf "Lang %S" payload
    | Attr t ->
        t#region, sprintf "Attr %S" t#payload

    (* Symbols *)

    | ARROW    t -> t#region, "ARROW"
    | CONS     t -> t#region, "CONS"
    | CARET    t -> t#region, "CARET"
    | MINUS    t -> t#region, "MINUS"
    | PLUS     t -> t#region, "PLUS"
    | SLASH    t -> t#region, "SLASH"
    | TIMES    t -> t#region, "TIMES"
    | LPAR     t -> t#region, "LPAR"
    | RPAR     t -> t#region, "RPAR"
    | LBRACKET t -> t#region, "LBRACKET"
    | RBRACKET t -> t#region, "RBRACKET"
    | LBRACE   t -> t#region, "LBRACE"
    | RBRACE   t -> t#region, "RBRACE"
    | COMMA    t -> t#region, "COMMA"
    | SEMI     t -> t#region, "SEMI"
    | VBAR     t -> t#region, "VBAR"
    | COLON    t -> t#region, "COLON"
    | DOT      t -> t#region, "DOT"
    | WILD     t -> t#region, "WILD"
    | EQ       t -> t#region, "EQ"
    | NE       t -> t#region, "NE"
    | LT       t -> t#region, "LT"
    | GT       t -> t#region, "GT"
    | LE       t -> t#region, "LE"
    | GE       t -> t#region, "GE"
    | BOOL_OR  t -> t#region, "BOOL_OR"
    | BOOL_AND t -> t#region, "BOOL_AND"
    | QUOTE    t -> t#region, "QUOTE"

    (* Keywords *)

    | Begin  t -> t#region, "Begin"
    | Else   t -> t#region, "Else"
    | End    t -> t#region, "End"
    | Fun    t -> t#region, "Fun"
    | Rec    t -> t#region, "Rec"
    | If     t -> t#region, "If"
    | In     t -> t#region, "In"
    | Let    t -> t#region, "Let"
    | Match  t -> t#region, "Match"
    | Mod    t -> t#region, "Mod"
    | Land   t -> t#region, "Land"
    | Lor    t -> t#region, "Lor"
    | Lxor   t -> t#region, "Lxor"
    | Lsl    t -> t#region, "Lsl"
    | Lsr    t -> t#region, "Lsr"
    | Not    t -> t#region, "Not"
    | Of     t -> t#region, "Of"
    | Or     t -> t#region, "Or"
    | Then   t -> t#region, "Then"
    | Type   t -> t#region, "Type"
    | With   t -> t#region, "With"
    | Module t -> t#region, "Module"
    | Struct t -> t#region, "Struct"

    (* End-Of-File *)

    | EOF t -> t#region, "EOF"

    (* From tokens to lexemes *)

    let to_lexeme = function
      (* Directives *)

      Directive d -> Directive.to_lexeme d

      (* Literals *)

    | String t   -> sprintf "%S" (String.escaped t#payload)
    | Verbatim t -> String.escaped t#payload
    | Bytes t    -> fst t#payload
    | Int t
    | Nat t
    | Mutez t    -> fst t#payload
    | Ident t    -> t#payload
    | UIdent t   -> t#payload
    | Attr t     -> sprintf "[@%s]" t#payload
    | Lang {value = {value = payload; _}; _}  -> payload

    (* Symbols *)

    | ARROW    _ -> "->"
    | CONS     _ -> "::"
    | CARET    _ -> "^"
    | MINUS    _ -> "-"
    | PLUS     _ -> "+"
    | SLASH    _ -> "/"
    | TIMES    _ -> "*"
    | LPAR     _ -> "("
    | RPAR     _ -> ")"
    | LBRACKET _ -> "["
    | RBRACKET _ -> "]"
    | LBRACE   _ -> "{"
    | RBRACE   _ -> "}"
    | COMMA    _ -> ","
    | SEMI     _ -> ";"
    | VBAR     _ -> "|"
    | COLON    _ -> ":"
    | DOT      _ -> "."
    | WILD     _ -> "_"
    | EQ       _ -> "="
    | NE       _ -> "<>"
    | LT       _ -> "<"
    | GT       _ -> ">"
    | LE       _ -> "<="
    | GE       _ -> ">="
    | BOOL_OR  _ -> "||"
    | BOOL_AND _ -> "&&"
    | QUOTE    _ -> "'"

    (* Keywords *)

    | Begin  _ -> "begin"
    | Else   _ -> "else"
    | End    _ -> "end"
    | Fun    _ -> "fun"
    | Rec    _ -> "rec"
    | If     _ -> "if"
    | In     _ -> "in"
    | Let    _ -> "let"
    | Match  _ -> "match"
    | Mod    _ -> "mod"
    | Land   _ -> "land"
    | Lor    _ -> "lor"
    | Lxor   _ -> "lxor"
    | Lsl    _ -> "lsl"
    | Lsr    _ -> "lsr"
    | Not    _ -> "not"
    | Of     _ -> "of"
    | Or     _ -> "or"
    | Type   _ -> "type"
    | Then   _ -> "then"
    | With   _ -> "with"
    | Module _ -> "module"
    | Struct _ -> "struct"

    (* End-Of-File *)

    | EOF _ -> ""


    (* CONVERSIONS *)

    let to_string ~offsets mode token =
      let region, val_str = proj_token token in
      let reg_str = region#compact ~offsets mode
      in sprintf "%s> %s" reg_str val_str

    let to_region token = proj_token token |> fst

    (* SMART CONSTRUCTORS *)

    (* Keywords *)

    let keywords = [
      (fun reg -> Begin     (wrap "begin" reg));
      (fun reg -> Else      (wrap "else" reg));
      (fun reg -> End       (wrap "fnd" reg));
      (fun reg -> Fun       (wrap "fun" reg));
      (fun reg -> Rec       (wrap "rec" reg));
      (fun reg -> If        (wrap "if" reg));
      (fun reg -> In        (wrap "in" reg));
      (fun reg -> Let       (wrap "let" reg));
      (fun reg -> Match     (wrap "match" reg));
      (fun reg -> Mod       (wrap "mod" reg));
      (fun reg -> Land      (wrap "land" reg));
      (fun reg -> Lor       (wrap "lor" reg));
      (fun reg -> Lxor      (wrap "lxor" reg));
      (fun reg -> Lsl       (wrap "lsl" reg));
      (fun reg -> Lsr       (wrap "lsr" reg));
      (fun reg -> Not       (wrap "not" reg));
      (fun reg -> Of        (wrap "of" reg));
      (fun reg -> Or        (wrap "or" reg));
      (fun reg -> Then      (wrap "then" reg));
      (fun reg -> Type      (wrap "type" reg));
      (fun reg -> With      (wrap "with" reg));
      (fun reg -> Module    (wrap "module" reg));
      (fun reg -> Struct    (wrap "struct" reg))
    ]

    let keywords =
      let add map (key, value) = SMap.add key value map in
      let apply map mk_kwd =
        add map (to_lexeme (mk_kwd Region.ghost), mk_kwd)
      in List.fold_left apply SMap.empty keywords

    type kwd_err = Invalid_keyword

    let mk_kwd ident region =
      match SMap.find_opt ident keywords with
        Some mk_kwd -> Ok (mk_kwd region)
      |        None -> Error Invalid_keyword

    (* Directives *)

    let mk_directive dir = Directive dir

    (* Strings *)

    let mk_string lexeme region = String (wrap lexeme region)

    let mk_verbatim lexeme region = Verbatim (wrap lexeme region)

    (* Bytes *)

    let mk_bytes lexeme region =
      let norm = Str.(global_replace (regexp "_") "" lexeme) in
      let value = lexeme, `Hex norm
      in Bytes (wrap value region)

    (* Numerical values *)

    type int_err = Non_canonical_zero

    let mk_int lexeme region =
      let z =
        Str.(global_replace (regexp "_") "" lexeme) |> Z.of_string
      in if   Z.equal z Z.zero && lexeme <> "0"
         then Error Non_canonical_zero
         else Ok (Int (wrap (lexeme, z) region))

    type nat_err =
      Invalid_natural
    | Unsupported_nat_syntax
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
          else Ok (Nat (wrap (lexeme, z) region))

    type mutez_err =
      Unsupported_mutez_syntax
    | Non_canonical_zero_tez

    let mk_mutez lexeme region =
      let z = Str.(global_replace (regexp "_") "" lexeme) |>
                Str.(global_replace (regexp "mutez") "") |>
                Z.of_string in
      if   Z.equal z Z.zero && lexeme <> "0mutez"
      then Error Non_canonical_zero_tez
      else Ok (Mutez (wrap (lexeme, z) region))

    (* End-Of-File *)

    let mk_eof region = EOF (wrap "" region)

    (* Symbols *)

    type sym_err = Invalid_symbol of string

    let mk_sym lexeme region =
      match lexeme with
        (* Lexemes in common with all concrete syntaxes *)

        ";"   -> Ok (SEMI     (wrap lexeme region))
      | ","   -> Ok (COMMA    (wrap lexeme region))
      | "("   -> Ok (LPAR     (wrap lexeme region))
      | ")"   -> Ok (RPAR     (wrap lexeme region))
      | "["   -> Ok (LBRACKET (wrap lexeme region))
      | "]"   -> Ok (RBRACKET (wrap lexeme region))
      | "{"   -> Ok (LBRACE   (wrap lexeme region))
      | "}"   -> Ok (RBRACE   (wrap lexeme region))
      | "="   -> Ok (EQ       (wrap lexeme region))
      | ":"   -> Ok (COLON    (wrap lexeme region))
      | "|"   -> Ok (VBAR     (wrap lexeme region))
      | "."   -> Ok (DOT      (wrap lexeme region))
      | "_"   -> Ok (WILD     (wrap lexeme region))
      | "+"   -> Ok (PLUS     (wrap lexeme region))
      | "-"   -> Ok (MINUS    (wrap lexeme region))
      | "*"   -> Ok (TIMES    (wrap lexeme region))
      | "/"   -> Ok (SLASH    (wrap lexeme region))
      | "<"   -> Ok (LT       (wrap lexeme region))
      | "<="  -> Ok (LE       (wrap lexeme region))
      | ">"   -> Ok (GT       (wrap lexeme region))
      | ">="  -> Ok (GE       (wrap lexeme region))

      (* Lexemes specific to CameLIGO *)

      | "^"   -> Ok (CARET    (wrap lexeme region))
      | "<>"  -> Ok (NE       (wrap lexeme region))
      | "::"  -> Ok (CONS     (wrap lexeme region))
      | "->"  -> Ok (ARROW    (wrap lexeme region))
      | "||"  -> Ok (BOOL_OR  (wrap lexeme region))
      | "&&"  -> Ok (BOOL_AND (wrap lexeme region))
      | "'"   -> Ok (QUOTE    (wrap lexeme region))

      (* Invalid symbols *)

      | s ->  Error (Invalid_symbol s)

    (* Identifiers *)

    let mk_ident value region =
      match SMap.find_opt value keywords with
        Some mk_kwd -> mk_kwd region
      |        None -> Ident (wrap value region)

    (* Constructors/Modules *)

    let mk_uident value region = UIdent (wrap value region)

    (* Attributes *)

    let mk_attr lexeme region = Attr (wrap lexeme region)

    (* Code injection *)

    type lang_err = Unsupported_lang_syntax

    let mk_lang lang region = Ok (Lang Region.{value=lang; region})
    (* let mk_lang lang region = Ok (Lang (wrap lang region)) *)

    (* PREDICATES *)

    let is_eof = function EOF _ -> true | _ -> false

    let support_string_delimiter c = (c = '"')

    let verbatim_delimiters = ("{|", "|}")
  end

include T

module type S = module type of T
