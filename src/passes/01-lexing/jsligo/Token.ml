(* Token specification for JsLIGO *)

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

      (* Comments *)

    | BlockCom of lexeme wrap
    | LineCom  of lexeme wrap

      (* Literals *)

    | String   of lexeme wrap
    | Verbatim of lexeme wrap
    | Bytes    of (lexeme * Hex.t) wrap
    | Int      of (lexeme * Z.t) wrap
   (* | Nat      of (lexeme * Z.t) *)
   (* | Mutez    of (lexeme * Z.t) *)
    | Ident   of lexeme wrap
    | UIdent   of lexeme wrap
   (* | Lang     of lexeme reg*)
    | Attr     of string wrap

    (* Symbols *)

    | MINUS    of lexeme wrap  (* "-"    *)
    | PLUS     of lexeme wrap  (* "+"    *)
    | SLASH    of lexeme wrap (* "/"    *)
    | TIMES    of lexeme wrap  (* "*"    *)
    | REM      of lexeme wrap  (* "%"    *)
    (*| PLUS2    of Region.t  (* "++"   *)
    | MINUS2   of Region.t  (* "--"   *) *)

    | LPAR     of lexeme wrap  (* "("    *)
    | RPAR     of lexeme wrap  (* ")"    *)
    | LBRACKET of lexeme wrap  (* "["    *)
    | RBRACKET of lexeme wrap  (* "]"    *)
    | LBRACE   of lexeme wrap  (* "{"    *)
    | RBRACE   of lexeme wrap  (* "}"    *)

    | COMMA    of lexeme wrap  (* ","    *)
    | SEMI     of lexeme wrap  (* ";"    *)
    | COLON    of lexeme wrap  (* ":"    *)
    | DOT      of lexeme wrap (* "."    *)
    | ELLIPSIS of lexeme wrap (* "..."  *)

    | BOOL_OR  of lexeme wrap (* "||"   *)
    | BOOL_AND of lexeme wrap (* "&&"   *)
    | BOOL_NOT of lexeme wrap (* "!"    *)

    (*| BIT_AND  of Region.t  (* "&"    *)
    | BIT_NOT  of Region.t  (* "~"    *)
    | BIT_XOR  of Region.t  (* "^"    *)
    | SHIFT_L  of Region.t  (* "<<<"  *)
    | SHIFT_R  of Region.t  (* ">>>"  *) *)

    | EQ       of lexeme wrap (* "="    *)
    | EQ2      of lexeme wrap (* "=="  *)
    | NE       of lexeme wrap (* "!="  *)

    | LT       of lexeme wrap (* "<"    *)
    | GT       of lexeme wrap (* ">"    *)
    | LE       of lexeme wrap (* "<="   *)
    | GE       of lexeme wrap (* ">="   *)

    | PLUS_EQ  of lexeme wrap (* "+="   *)
    | MINUS_EQ of lexeme wrap (* "-="   *)
    | MULT_EQ  of lexeme wrap (* "*="   *)
    | REM_EQ   of lexeme wrap (* "%="   *)
    | DIV_EQ   of lexeme wrap (* "/="   *)
    (* | SL_EQ    of Region.t  (* "<<<=" *)
    | SR_EQ    of Region.t  (* ">>>=" *)
    | AND_EQ   of Region.t  (* "&="   *)
    | OR_EQ    of Region.t  (* "|="   *)
    | XOR_EQ   of Region.t  (* "^="   *) *)

    | VBAR     of lexeme wrap (* "|"    *)
    | ARROW    of lexeme wrap (* "=>"   *)
    | WILD     of lexeme wrap (* "_"    *)

    (* JavaScript Keywords *)

    (* | Break    of Region.t  (* break    *) *)
    | Case     of lexeme wrap (* case     *)
    (* | Class    of Region.t  (* class    *) *)
    | Const    of lexeme wrap (* const    *)
    | Default  of lexeme wrap (* default  *)
    | Else     of lexeme wrap (* else     *)
    | Export   of lexeme wrap (* export   *)
    | For      of lexeme wrap (* for      *)
    | If       of lexeme wrap (* if       *)
    | Import   of lexeme wrap (* import   *)
    | Let      of lexeme wrap (* let      *)
    | Of       of lexeme wrap (* of       *)
    | Return   of lexeme wrap (* return   *)
    | Break    of lexeme wrap (* break    *)
    | Switch   of lexeme wrap (* switch   *)
    (* | This     of Region.t  (* this     *) *)
    (* | Void     of Region.t  (* void     *) *)
    | While    of lexeme wrap (* while    *)
    (* | With     of Region.t  (* with     *)  *)

    (* TypeScript keywords *)

    | As          of lexeme wrap (* as          *)
    | Namespace   of lexeme wrap (* namespace   *)
    | Type        of lexeme wrap (* type        *)

    (* Virtual tokens *)

    | ZWSP of lexeme wrap (* Zero-Width SPace *)

    (* End-Of-File *)

    | EOF of lexeme wrap

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
    | "UIdent"   -> ctor_sym ()
    | "Int"      -> "1"
    (* | "Nat"      -> "1n" *)
    (* | "Mutez"    -> "1mutez" *)
    | "String"   -> "\"a string\""
    | "Verbatim" -> "{|verbatim|}"
    | "Bytes"    -> "0xAA"
    | "Attr"     -> "[@attr]"
    (* | "Lang"     -> "[%Michelson" *)

    (* Symbols *)

    | "MINUS"    -> "-"
    | "PLUS"     -> "+"
    | "SLASH"    -> "/"
    | "TIMES"    -> "*"
    | "REM"      -> "%"
    (* | "PLUS2"    -> "++" *)
    (* | "MINUS2"   -> "--" *)

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

    | "BOOL_OR"  -> "||"
    | "BOOL_AND" -> "&&"
    | "BOOL_NOT" -> "!"

    (* | "BIT_AND"  -> "&"
    | "BIT_NOT"  -> "~"
    | "BIT_XOR"  -> "^"
    | "SHIFT_L"  -> "<<<"
    | "SHIFT_R"  -> ">>>" *)

    | "EQ"       -> "="
    | "EQ2"      -> "=="
    | "NE"       -> "!="

    | "LT"       -> "<"
    | "GT"       -> ">"
    | "LE"       -> "<="
    | "GE"       -> ">="

    | "PLUS_EQ"  -> "+="
    | "MINUS_EQ" -> "-="
    | "MULT_EQ"  -> "*="
    | "REM_EQ"   -> "%="
    | "DIV_EQ"   -> "/="
    (* | "SL_EQ"    -> "<<<="
    | "SR_EQ"    -> ">>>="
    | "AND_EQ"   -> "&="
    | "OR_EQ"    -> "|="
    | "XOR_EQ"   -> "^=" *)

    | "VBAR"     -> "|"
    | "ARROW"    -> "=>"
    | "WILD"     -> "_"

    (* JavaScript Keywords *)

    (* | "Break"    -> "break" *)
    | "Case"     -> "case"
    (* | "Class"    -> "class" *)
    | "Const"    -> "const"
    | "Default"  -> "default"
    | "Else"     -> "else"
    | "Export"   -> "export"
    | "For"      -> "for"
    | "If"       -> "if"
    | "Import"   -> "import"
    | "Let"      -> "let"
    | "Of"       -> "of"
    | "Return"   -> "return"
    | "Break"    -> "break"
    | "Switch"   -> "switch"
    (* | "This"     -> "this" *)
    (* | "Void"     -> "void" *)
    | "While"    -> "while"
    (* | "With"     -> "with" *)

    (* TypeScript keywords *)

    | "Type"      -> "type"
    | "Namespace" -> "namespace"
    | "As"        -> "as"

    (* Virtual tokens *)

    | "ZWSP" -> ""

    (* End-Of-File *)

    | "EOF" -> ""

    (* This case should not happen! *)

    | _  -> "\\Unknown" (* Backslash meant to trigger an error *)

    (* Projections *)

    let sprintf = Printf.sprintf

    type token = t

    let proj_token = function
        (* Preprocessing directives *)

      Directive d -> Directive.project d

      (* Comments *)

    | LineCom t ->
        t#region, sprintf "Line comment %S" t#payload
    | BlockCom t ->
        t#region, sprintf "Block comment %S" t#payload

      (* Literals *)

    | String t ->
        t#region, sprintf "String %S" t#payload
    | Verbatim t ->
        t#region, sprintf "Verbatim %S" t#payload
    | Bytes t ->
        let (s, b) = t#payload in
        t#region,
        sprintf "Bytes (%S, \"0x%s\")" s (Hex.show b)
    | Int t ->
        let (s, n) = t#payload in
        t#region, sprintf "Int (%S, %s)" s (Z.to_string n)
    (* | Nat Region.{w_region=region; value = s,n} ->
        region, sprintf "Nat (%S, %s)" s (Z.to_string n)
    | Mutez Region.{w_region=region; value = s,n} ->
        region, sprintf "Mutez (%S, %s)" s (Z.to_string n) *)
    | Ident t ->
        t#region, sprintf "Ident %S" t#payload
    | UIdent t ->
        t#region, sprintf "UIdent %S" t#payload
    (* | Lang Region.{w_region=region; value} ->
        region, sprintf "Lang %S" (value.Region.value) *)
    | Attr t ->
        t#region, sprintf "Attr %S" t#payload

    (* Symbols *)

    | MINUS    t -> t#region, "MINUS"
    | PLUS     t -> t#region, "PLUS"
    | SLASH    t -> t#region, "SLASH"
    | TIMES    t -> t#region, "TIMES"
    | REM      t -> t#region, "REM"
    (* | PLUS2    t -> t#region, "PLUS2"
    | MINUS2   t -> t#region, "MINUS2" *)

    | LPAR     t -> t#region, "LPAR"
    | RPAR     t -> t#region, "RPAR"
    | LBRACKET t -> t#region, "LBRACKET"
    | RBRACKET t -> t#region, "RBRACKET"
    | LBRACE   t -> t#region, "LBRACE"
    | RBRACE   t -> t#region, "RBRACE"

    | COMMA    t -> t#region, "COMMA"
    | SEMI     t -> t#region, "SEMI"
    | COLON    t -> t#region, "COLON"
    | DOT      t -> t#region, "DOT"
    | ELLIPSIS t -> t#region, "ELLIPSIS"

    | BOOL_OR  t -> t#region, "BOOL_OR"
    | BOOL_AND t -> t#region, "BOOL_AND"
    | BOOL_NOT t -> t#region, "BOOL_NOT"

    (* | BIT_AND  t -> t#region, "BIT_AND"
    | BIT_NOT  t -> t#region, "BIT_NOT"
    | BIT_XOR  t -> t#region, "BIT_XOR"
    | SHIFT_L  t -> t#region, "SHIFT_L"
    | SHIFT_R  t -> t#region, "SHIFT_R" *)

    | EQ       t -> t#region, "EQ"
    | EQ2      t -> t#region, "EQ2"
    | NE       t -> t#region, "NE"

    | LT       t -> t#region, "LT"
    | GT       t -> t#region, "GT"
    | LE       t -> t#region, "LE"
    | GE       t -> t#region, "GE"

    | PLUS_EQ  t -> t#region, "PLUS_EQ"
    | MINUS_EQ t -> t#region, "MINUS_EQ"
    | MULT_EQ  t -> t#region, "MULT_EQ"
    | REM_EQ   t -> t#region, "REM_EQ"
    | DIV_EQ   t -> t#region, "DIV_EQ"
    (* | SL_EQ    t -> t#region, "SL_EQ"
    | SR_EQ    t -> t#region, "SR_EQ"
    | AND_EQ   t -> t#region, "AND_EQ"
    | OR_EQ    t -> t#region, "OR_EQ"
    | XOR_EQ   t -> t#region, "XOR_EQ" *)

    | VBAR     t -> t#region, "VBAR"
    | ARROW    t -> t#region, "ARROW"
    | WILD     t -> t#region, "WILD"

    (* JavaScript Keywords *)

    (* | Break    t -> t#region, "Break" *)
    | Case     t -> t#region, "Case"
    (* | Class    t -> t#region, "Class" *)
    | Const    t -> t#region, "Const"
    | Default  t -> t#region, "Default"
    | Else     t -> t#region, "Else"
    | Export   t -> t#region, "Export"
    | For      t -> t#region, "For"
    | If       t -> t#region, "If"
    | Import   t -> t#region, "Import"
    | Let      t -> t#region, "Let"
    | Of       t -> t#region, "Of"
    | Return   t -> t#region, "Return"
    | Break    t -> t#region, "Break"
    | Switch   t -> t#region, "Switch"
    (* | This     t -> t#region, "This" *)
    (* | Void     t -> t#region, "Void" *)
    | While    t -> t#region, "While"
    (* | With     t -> t#region, "With" *)

    (* TypeScript keywords *)

    | As          t -> t#region, "As"
    | Namespace   t -> t#region, "Namespace"
    | Type        t -> t#region, "Type"

    (* Virtual tokens *)

    | ZWSP t -> t#region, "ZWSP"

    (* End-Of-File *)

    | EOF t -> t#region, "EOF"


    let to_lexeme = function
      (* Directives *)

      Directive d -> Directive.to_lexeme d

      (* Comments *)

    | LineCom t  -> sprintf "// %s" t#payload
    | BlockCom t -> sprintf "/* %s */" t#payload

      (* Literals *)

    | String t   -> sprintf "%S" (String.escaped t#payload)
    | Verbatim t -> String.escaped t#payload
    | Bytes t    -> fst t#payload
    | Int t      -> fst t#payload
    | Ident t    -> t#payload
    | UIdent t   -> t#payload
    | Attr t     -> sprintf "[@%s]" t#payload
    (* | Lang lang  -> Region.(lang.value.value) *)

    (* Symbols *)

    | MINUS    _ -> "-"
    | PLUS     _ -> "+"
    | SLASH    _ -> "/"
    | TIMES    _ -> "*"
    | REM      _ -> "%"
    (* | PLUS2    _ -> "++"
    | MINUS2   _ -> "--" *)

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

    | BOOL_OR  _ -> "||"
    | BOOL_AND _ -> "&&"
    | BOOL_NOT _ -> "!"

    (* | BIT_AND  _ -> "&"
    | BIT_NOT  _ -> "~"
    | BIT_XOR  _ -> "^"
    | SHIFT_L  _ -> "<<<"
    | SHIFT_R  _ -> ">>>" *)

    | EQ       _ -> "="
    | EQ2      _ -> "=="
    | NE       _ -> "!="

    | LT       _ -> "<"
    | GT       _ -> ">"
    | LE       _ -> "<="
    | GE       _ -> ">="

    | PLUS_EQ  _ -> "+="
    | MINUS_EQ _ -> "-="
    | MULT_EQ  _ -> "*="
    | REM_EQ   _ -> "%="
    | DIV_EQ   _ -> "/="
    (* | SL_EQ    _ -> "<<<="
    | SR_EQ    _ -> ">>>="
    | AND_EQ   _ -> "&="
    | OR_EQ    _ -> "|="
    | XOR_EQ   _ -> "^=" *)

    | VBAR     _ -> "|"
    | ARROW    _ -> "=>"
    | WILD     _ -> "_"

    (* JavaScript Keywords *)

    (* | Break    _ -> "break" *)
    | Case     _ -> "case"
    (* | Class    _ -> "class" *)
    | Const    _ -> "const"
    | Default  _ -> "default"
    | Else     _ -> "else"
    | Export   _ -> "export"
    | For      _ -> "for"
    | If       _ -> "if"
    | Import   _ -> "import"
    | Let      _ -> "let"
    | Of       _ -> "of"
    | Return   _ -> "return"
    | Break    _ -> "break"
    | Switch   _ -> "switch"
    (* | This     _ -> "this" *)
    (* | Void     _ -> "void" *)
    | While    _ -> "while"
    (* | With     _ -> "with" *)

    (* TypeScript keywords *)

    | As        _ -> "as"
    | Namespace _ -> "namespace"
    | Type      _ -> "type"

    (* Virtual tokens *)

    | ZWSP _ -> ""

    (* End-Of-File *)

    | EOF _ -> ""

    (* CONVERSIONS *)

    let to_string ~offsets mode token =
      let region, val_str = proj_token token in
      let reg_str = region#compact ~offsets mode
      in sprintf "%s: %s" reg_str val_str

    let to_region token = proj_token token |> fst

    (* SMART CONSTRUCTORS *)

    (* Keywords *)

    let keywords = [
        (* JavaScript Keywords *)

       (* (fun reg -> Break   (wrap reg)); *)
       (fun reg -> Case    (wrap "case"         reg));
       (* (fun reg -> Class   (wrap reg)); *)
       (fun reg -> Const   (wrap "const"        reg));
       (fun reg -> Else    (wrap "else"         reg));
       (fun reg -> Default (wrap "default"      reg));
       (fun reg -> Export  (wrap "export"       reg));
       (fun reg -> For     (wrap "for"          reg));
       (fun reg -> If      (wrap "if"           reg));
       (fun reg -> Import  (wrap "import"       reg));
       (fun reg -> Let     (wrap "let"          reg));
       (fun reg -> Of      (wrap "of"           reg));
       (fun reg -> Return  (wrap "return"       reg));
       (fun reg -> Break   (wrap "break"        reg));
       (fun reg -> Switch  (wrap "switch"       reg));
       (* (fun reg -> This    (wrap reg)); *)
       (* (fun reg -> Void    (wrap reg)); *)
       (fun reg -> While   (wrap "while"        reg));
       (* (fun reg -> With    (wrap reg)); *)

       (* TypeScript keywords *)

       (fun reg -> As        (wrap "as"         reg));
       (fun reg -> Namespace (wrap "namespace"  reg));
       (fun reg -> Type      (wrap "type"       reg));
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

    (* Strings *)

    let mk_string lexeme region = String (wrap lexeme region)

    (* Verbatim strings *)

    let mk_verbatim lexeme region = Verbatim (wrap lexeme region)

    (* Bytes *)

    let mk_bytes lexeme region =
      let norm = Str.(global_replace (regexp "_") "" lexeme) in
      let value = lexeme, `Hex norm
      in Bytes (wrap value region)

    (* Integers *)

    let mk_int lexeme z region = Int (wrap (lexeme, z) region)

    (* Natural numbers *)

    type nat_err = Wrong_nat_syntax of string

    let mk_nat _nat _z _region =
      Error (Wrong_nat_syntax "Example: \"12334 as nat\".")

    (* Mutez *)

    type mutez_err = Wrong_mutez_syntax of string

    let mk_mutez _nat ~suffix:_ _int64 _region =
      Error (Wrong_mutez_syntax "Example: \"1234 as mutez\".")

    (* End-Of-File *)

    let mk_eof region = EOF (wrap "" region)

    (* Symbol *)

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

    (* Symbols specific to JsLIGO *)

      | "%"   -> Ok (REM      (wrap lexeme region))
    (* | "++"  -> Ok (PLUS2    region)
      | "--"  -> Ok (MINUS2   region) *)

      | "..." -> Ok (ELLIPSIS (wrap lexeme region))

      | "||"  -> Ok (BOOL_OR  (wrap lexeme region))
      | "&&"  -> Ok (BOOL_AND (wrap lexeme region))
      | "!"   -> Ok (BOOL_NOT (wrap lexeme region))

    (* | "&"   -> Ok (BIT_AND  region)
      | "~"   -> Ok (BIT_NOT  region)
      | "^"   -> Ok (BIT_XOR  region)
      | "<<<" -> Ok (SHIFT_L  region)
      | ">>>" -> Ok (SHIFT_R  region) *)

      | "==" -> Ok (EQ2      (wrap lexeme region))
      | "!=" -> Ok (NE       (wrap lexeme region))

      | "+="  -> Ok (PLUS_EQ  (wrap lexeme region))
      | "-="  -> Ok (MINUS_EQ (wrap lexeme region))
      | "*="  -> Ok (MULT_EQ  (wrap lexeme region))
      | "%="  -> Ok (REM_EQ   (wrap lexeme region))
      | "/="   -> Ok (DIV_EQ  (wrap lexeme region))

   (* | "<<<=" -> Ok (SL_EQ   region)
      | ">>>=" -> Ok (SR_EQ   region)
      | "&="   -> Ok (AND_EQ  region)
      | "|="   -> Ok (OR_EQ   region)
      | "^="   -> Ok (XOR_EQ  region) *)

      | "=>"   -> Ok (ARROW   (wrap lexeme region))

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

    type lang_err = Wrong_lang_syntax of string

    let mk_lang _lang _region =
      Error (Wrong_lang_syntax
               "Example: \"(Michelson `{UNPAIR; ADD}`\
                         as ((n: [nat, nat]) => nat))\".")

    (* PREDICATES *)

    let is_eof = function EOF _ -> true | _ -> false

    let support_string_delimiter c = (c = '"')

    let verbatim_delimiters = ("`", "`")
  end

include T

module type S = module type of T
