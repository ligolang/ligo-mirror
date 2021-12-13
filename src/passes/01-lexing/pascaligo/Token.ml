(* Token specification for PascaLIGO *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module Markup    = LexerLib.Markup
module Directive = LexerLib.Directive

(* Utility modules and types *)

module SMap = Map.Make (String)
module Wrap = Lexing_shared.Wrap
module Attr = Lexing_shared.Attr

let sprintf = Printf.sprintf

let wrap = Wrap.wrap

module T =
  struct
    (* A lexeme is the concrete syntax of a token *)

    type lexeme = string

    (* Definition of tokens generated by "menhir --only-tokens"

       It contains [token] and ['a terminal] types. The first one we
       redefine manually here (by type [t]) but the second one we need
       to satisfy Menhir's Inspection API.  *)

    include Menhir_pascaligo_tokens.MenhirToken

    (* TOKENS *)

    type t =
      (* Preprocessing directives *)

      Directive of Directive.t

      (* Literals *)

    | String   of lexeme Wrap.t
    | Verbatim of lexeme Wrap.t
    | Bytes    of (lexeme * Hex.t) Wrap.t
    | Int      of (lexeme * Z.t) Wrap.t
    | Nat      of (lexeme * Z.t) Wrap.t
    | Mutez    of (lexeme * Int64.t) Wrap.t
    | Ident    of lexeme Wrap.t
    | UIdent   of lexeme Wrap.t
    | Lang     of lexeme Region.reg Region.reg
    | Attr     of Attr.t Region.reg

    (* Symbols *)

    | SEMI     of lexeme Wrap.t  (* ;   *)
    | COMMA    of lexeme Wrap.t  (* ,   *)
    | LPAR     of lexeme Wrap.t  (* (   *)
    | RPAR     of lexeme Wrap.t  (* )   *)
    | LBRACE   of lexeme Wrap.t  (* {   *)
    | RBRACE   of lexeme Wrap.t  (* }   *)
    | LBRACKET of lexeme Wrap.t  (* [   *)
    | RBRACKET of lexeme Wrap.t  (* ]   *)
    | SHARP    of lexeme Wrap.t  (* #   *)
    | VBAR     of lexeme Wrap.t  (* |   *)
    | ARROW    of lexeme Wrap.t  (* ->  *)
    | ASS      of lexeme Wrap.t  (* :=  *)
    | EQ       of lexeme Wrap.t  (* =   *)
    | COLON    of lexeme Wrap.t  (* :   *)
    | LT       of lexeme Wrap.t  (* <   *)
    | LE       of lexeme Wrap.t  (* <=  *)
    | GT       of lexeme Wrap.t  (* >   *)
    | GE       of lexeme Wrap.t  (* >=  *)
    | NE       of lexeme Wrap.t  (* =/= *)
    | PLUS     of lexeme Wrap.t  (* +   *)
    | MINUS    of lexeme Wrap.t  (* -   *)
    | SLASH    of lexeme Wrap.t  (* /   *)
    | TIMES    of lexeme Wrap.t  (* *   *)
    | DOT      of lexeme Wrap.t  (* .   *)
    | WILD     of lexeme Wrap.t  (* _   *)
    | CARET    of lexeme Wrap.t  (* ^   *)

    (* Keywords *)

    | And        of lexeme Wrap.t  (* and       *)
    | Begin      of lexeme Wrap.t  (* begin     *)
    | BigMap     of lexeme Wrap.t  (* big_map   *)
    | Block      of lexeme Wrap.t  (* block     *)
    | Case       of lexeme Wrap.t  (* case      *)
    | Const      of lexeme Wrap.t  (* const     *)
    | Contains   of lexeme Wrap.t  (* contains  *)
    | Else       of lexeme Wrap.t  (* else      *)
    | End        of lexeme Wrap.t  (* end       *)
    | For        of lexeme Wrap.t  (* for       *)
    | From       of lexeme Wrap.t  (* from      *)
    | Function   of lexeme Wrap.t  (* function  *)
    | If         of lexeme Wrap.t  (* if        *)
    | In         of lexeme Wrap.t  (* in        *)
    | Is         of lexeme Wrap.t  (* is        *)
    | List       of lexeme Wrap.t  (* list      *)
    | Map        of lexeme Wrap.t  (* map       *)
    | Mod        of lexeme Wrap.t  (* mod       *)
    | Module     of lexeme Wrap.t  (* module    *)
    | Nil        of lexeme Wrap.t  (* nil       *)
    | Not        of lexeme Wrap.t  (* not       *)
    | Of         of lexeme Wrap.t  (* of        *)
    | Or         of lexeme Wrap.t  (* or        *)
    | Patch      of lexeme Wrap.t  (* patch     *)
    | Record     of lexeme Wrap.t  (* record    *)
    | Recursive  of lexeme Wrap.t  (* recursive *)
    | Remove     of lexeme Wrap.t  (* remove    *)
    | Set        of lexeme Wrap.t  (* set       *)
    | Skip       of lexeme Wrap.t  (* skip      *)
    | Step       of lexeme Wrap.t  (* step      *)
    | Then       of lexeme Wrap.t  (* then      *)
    | To         of lexeme Wrap.t  (* to        *)
    | Type       of lexeme Wrap.t  (* type      *)
    | Var        of lexeme Wrap.t  (* var       *)
    | While      of lexeme Wrap.t  (* while     *)
    | With       of lexeme Wrap.t  (* with      *)

    (* End-Of-File *)

    | EOF of lexeme Wrap.t

    type token = t


    (* FROM TOKENS TO LEXEMES *)

    let to_lexeme = function
      (* Directives *)

      Directive d -> Directive.to_lexeme d

      (* Literals *)

    | String t   -> sprintf "%S" t#payload (* Escaped *)
    | Verbatim t -> String.escaped t#payload
    | Bytes t    -> fst t#payload
    | Int t
    | Nat t      -> fst t#payload
    | Mutez t    -> fst t#payload
    | Ident t
    | UIdent t   -> t#payload
    | Attr t     -> Attr.to_lexeme t.Region.value
    | Lang lang  -> "[%" ^ Region.(lang.value.value)

    (* Symbols *)

    | SEMI     t
    | COMMA    t
    | LPAR     t
    | RPAR     t
    | LBRACE   t
    | RBRACE   t
    | LBRACKET t
    | RBRACKET t
    | SHARP    t
    | VBAR     t
    | ARROW    t
    | ASS      t
    | EQ       t
    | COLON    t
    | LT       t
    | LE       t
    | GT       t
    | GE       t
    | NE       t
    | PLUS     t
    | MINUS    t
    | SLASH    t
    | TIMES    t
    | DOT      t
    | WILD     t
    | CARET    t

    (* Keywords *)

    | And       t
    | Begin     t
    | BigMap    t
    | Block     t
    | Case      t
    | Const     t
    | Contains  t
    | Else      t
    | End       t
    | For       t
    | From      t
    | Function  t
    | If        t
    | In        t
    | Is        t
    | List      t
    | Map       t
    | Mod       t
    | Module    t
    | Nil       t
    | Not       t
    | Of        t
    | Or        t
    | Patch     t
    | Record    t
    | Recursive t
    | Remove    t
    | Set       t
    | Skip      t
    | Step      t
    | Then      t
    | To        t
    | Type      t
    | Var       t
    | While     t
    | With      t -> t#payload

    (* End-Of-File *)

    | EOF _ -> ""


    (* KEYWORDS *)

    let wrap_and       = wrap "and"
    let wrap_begin     = wrap "begin"
    let wrap_big_map   = wrap "big_map"
    let wrap_block     = wrap "block"
    let wrap_case      = wrap "case"
    let wrap_const     = wrap "const"
    let wrap_contains  = wrap "contains"
    let wrap_else      = wrap "else"
    let wrap_end       = wrap "end"
    let wrap_for       = wrap "for"
    let wrap_from      = wrap "from"
    let wrap_function  = wrap "function"
    let wrap_if        = wrap "if"
    let wrap_in        = wrap "in"
    let wrap_is        = wrap "is"
    let wrap_list      = wrap "list"
    let wrap_map       = wrap "map"
    let wrap_mod       = wrap "mod"
    let wrap_module    = wrap "module"
    let wrap_nil       = wrap "nil"
    let wrap_not       = wrap "not"
    let wrap_of        = wrap "of"
    let wrap_or        = wrap "or"
    let wrap_patch     = wrap "patch"
    let wrap_record    = wrap "record"
    let wrap_recursive = wrap "recursive"
    let wrap_remove    = wrap "remove"
    let wrap_set       = wrap "set"
    let wrap_skip      = wrap "skip"
    let wrap_step      = wrap "step"
    let wrap_then      = wrap "then"
    let wrap_to        = wrap "to"
    let wrap_type      = wrap "type"
    let wrap_var       = wrap "var"
    let wrap_while     = wrap "while"
    let wrap_with      = wrap "with"

    (* Smart constructors *)

    let mk_And       region = And       (wrap_and       region)
    let mk_Begin     region = Begin     (wrap_begin     region)
    let mk_BigMap    region = BigMap    (wrap_big_map   region)
    let mk_Block     region = Block     (wrap_block     region)
    let mk_Case      region = Case      (wrap_case      region)
    let mk_Const     region = Const     (wrap_const     region)
    let mk_Contains  region = Contains  (wrap_contains  region)
    let mk_Else      region = Else      (wrap_else      region)
    let mk_End       region = End       (wrap_end       region)
    let mk_For       region = For       (wrap_for       region)
    let mk_From      region = From      (wrap_from      region)
    let mk_Function  region = Function  (wrap_function  region)
    let mk_If        region = If        (wrap_if        region)
    let mk_In        region = In        (wrap_in        region)
    let mk_Is        region = Is        (wrap_is        region)
    let mk_List      region = List      (wrap_list      region)
    let mk_Map       region = Map       (wrap_map       region)
    let mk_Mod       region = Mod       (wrap_mod       region)
    let mk_Module    region = Module    (wrap_module    region)
    let mk_Nil       region = Nil       (wrap_nil       region)
    let mk_Not       region = Not       (wrap_not       region)
    let mk_Of        region = Of        (wrap_of        region)
    let mk_Or        region = Or        (wrap_or        region)
    let mk_Patch     region = Patch     (wrap_patch     region)
    let mk_Record    region = Record    (wrap_record    region)
    let mk_Recursive region = Recursive (wrap_recursive region)
    let mk_Remove    region = Remove    (wrap_remove    region)
    let mk_Set       region = Set       (wrap_set       region)
    let mk_Skip      region = Skip      (wrap_skip      region)
    let mk_Step      region = Step      (wrap_step      region)
    let mk_Then      region = Then      (wrap_then      region)
    let mk_To        region = To        (wrap_to        region)
    let mk_Type      region = Type      (wrap_type      region)
    let mk_Var       region = Var       (wrap_var       region)
    let mk_While     region = While     (wrap_while     region)
    let mk_With      region = With      (wrap_with      region)

    (* All keyword smart constructors *)

    let keywords = [
      mk_And;
      mk_Begin;
      mk_BigMap;
      mk_Block;
      mk_Case;
      mk_Const;
      mk_Contains;
      mk_Else;
      mk_End;
      mk_For;
      mk_From;
      mk_Function;
      mk_If;
      mk_In;
      mk_Is;
      mk_List;
      mk_Map;
      mk_Mod;
      mk_Module;
      mk_Nil;
      mk_Not;
      mk_Of;
      mk_Or;
      mk_Patch;
      mk_Record;
      mk_Recursive;
      mk_Remove;
      mk_Set;
      mk_Skip;
      mk_Step;
      mk_Then;
      mk_To;
      mk_Type;
      mk_Var;
      mk_While;
      mk_With
    ]

    (* All keywords *)

    let keywords =
      let add map (key, data) =
        match SMap.add ~key ~data map with
          `Ok map -> map
        | `Duplicate -> map in
      let apply map mk_kwd =
        add map (to_lexeme (mk_kwd Region.ghost), mk_kwd)
      in List.fold_left ~f:apply ~init:SMap.empty keywords

    (* Ghost keywords *)

    let ghost_and       = wrap_and       Region.ghost
    let ghost_begin     = wrap_begin     Region.ghost
    let ghost_big_map   = wrap_big_map   Region.ghost
    let ghost_block     = wrap_block     Region.ghost
    let ghost_case      = wrap_case      Region.ghost
    let ghost_const     = wrap_const     Region.ghost
    let ghost_contains  = wrap_contains  Region.ghost
    let ghost_else      = wrap_else      Region.ghost
    let ghost_end       = wrap_end       Region.ghost
    let ghost_for       = wrap_for       Region.ghost
    let ghost_from      = wrap_from      Region.ghost
    let ghost_function  = wrap_function  Region.ghost
    let ghost_if        = wrap_if        Region.ghost
    let ghost_in        = wrap_in        Region.ghost
    let ghost_is        = wrap_is        Region.ghost
    let ghost_list      = wrap_list      Region.ghost
    let ghost_map       = wrap_map       Region.ghost
    let ghost_mod       = wrap_mod       Region.ghost
    let ghost_module    = wrap_module    Region.ghost
    let ghost_nil       = wrap_nil       Region.ghost
    let ghost_not       = wrap_not       Region.ghost
    let ghost_of        = wrap_of        Region.ghost
    let ghost_or        = wrap_or        Region.ghost
    let ghost_patch     = wrap_patch     Region.ghost
    let ghost_record    = wrap_record    Region.ghost
    let ghost_recursive = wrap_recursive Region.ghost
    let ghost_remove    = wrap_remove    Region.ghost
    let ghost_set       = wrap_set       Region.ghost
    let ghost_skip      = wrap_skip      Region.ghost
    let ghost_step      = wrap_step      Region.ghost
    let ghost_then      = wrap_then      Region.ghost
    let ghost_to        = wrap_to        Region.ghost
    let ghost_type      = wrap_type      Region.ghost
    let ghost_var       = wrap_var       Region.ghost
    let ghost_while     = wrap_while     Region.ghost
    let ghost_with      = wrap_with      Region.ghost

    let ghost_And       = And       ghost_and
    let ghost_Begin     = Begin     ghost_begin
    let ghost_BigMap    = BigMap    ghost_big_map
    let ghost_Block     = Block     ghost_block
    let ghost_Case      = Case      ghost_case
    let ghost_Const     = Const     ghost_const
    let ghost_Contains  = Contains  ghost_contains
    let ghost_Else      = Else      ghost_else
    let ghost_End       = End       ghost_end
    let ghost_For       = For       ghost_for
    let ghost_From      = From      ghost_from
    let ghost_Function  = Function  ghost_function
    let ghost_If        = If        ghost_if
    let ghost_In        = In        ghost_in
    let ghost_Is        = Is        ghost_is
    let ghost_List      = List      ghost_list
    let ghost_Map       = Map       ghost_map
    let ghost_Mod       = Mod       ghost_mod
    let ghost_Module    = Module    ghost_module
    let ghost_Nil       = Nil       ghost_nil
    let ghost_Not       = Not       ghost_not
    let ghost_Of        = Of        ghost_of
    let ghost_Or        = Or        ghost_or
    let ghost_Patch     = Patch     ghost_patch
    let ghost_Record    = Record    ghost_record
    let ghost_Recursive = Recursive ghost_recursive
    let ghost_Remove    = Remove    ghost_remove
    let ghost_Set       = Set       ghost_set
    let ghost_Skip      = Skip      ghost_skip
    let ghost_Step      = Step      ghost_step
    let ghost_Then      = Then      ghost_then
    let ghost_To        = To        ghost_to
    let ghost_Type      = Type      ghost_type
    let ghost_Var       = Var       ghost_var
    let ghost_While     = While     ghost_while
    let ghost_With      = With      ghost_with


    (* SYMBOLS *)

    let wrap_semi     = wrap ";"
    let wrap_comma    = wrap ","
    let wrap_lpar     = wrap "("
    let wrap_rpar     = wrap ")"
    let wrap_lbracket = wrap "["
    let wrap_rbracket = wrap "]"
    let wrap_lbrace   = wrap "{"
    let wrap_rbrace   = wrap "}"
    let wrap_eq       = wrap "="
    let wrap_colon    = wrap ":"
    let wrap_vbar     = wrap "|"
    let wrap_dot      = wrap "."
    let wrap_wild     = wrap "_"
    let wrap_plus     = wrap "+"
    let wrap_minus    = wrap "-"
    let wrap_times    = wrap "*"
    let wrap_slash    = wrap "/"
    let wrap_lt       = wrap "<"
    let wrap_le       = wrap "<="
    let wrap_gt       = wrap ">"
    let wrap_ge       = wrap ">="

    let wrap_caret    = wrap "^"
    let wrap_arrow    = wrap "->"
    let wrap_ne       = wrap "=/="
    let wrap_sharp    = wrap "#"
    let wrap_ass      = wrap ":="

    (* Smart constructors *)

    let mk_SEMI     region = SEMI     (wrap_semi     region)
    let mk_COMMA    region = COMMA    (wrap_comma    region)
    let mk_LPAR     region = LPAR     (wrap_lpar     region)
    let mk_RPAR     region = RPAR     (wrap_rpar     region)
    let mk_LBRACKET region = LBRACKET (wrap_lbracket region)
    let mk_RBRACKET region = RBRACKET (wrap_rbracket region)
    let mk_LBRACE   region = LBRACE   (wrap_lbrace   region)
    let mk_RBRACE   region = RBRACE   (wrap_rbrace   region)
    let mk_EQ       region = EQ       (wrap_eq       region)
    let mk_COLON    region = COLON    (wrap_colon    region)
    let mk_VBAR     region = VBAR     (wrap_vbar     region)
    let mk_DOT      region = DOT      (wrap_dot      region)
    let mk_WILD     region = WILD     (wrap_wild     region)
    let mk_PLUS     region = PLUS     (wrap_plus     region)
    let mk_MINUS    region = MINUS    (wrap_minus    region)
    let mk_TIMES    region = TIMES    (wrap_times    region)
    let mk_SLASH    region = SLASH    (wrap_slash    region)
    let mk_LT       region = LT       (wrap_lt       region)
    let mk_LE       region = LE       (wrap_le       region)
    let mk_GT       region = GT       (wrap_gt       region)
    let mk_GE       region = GE       (wrap_ge       region)

    let mk_CARET    region = CARET    (wrap_caret    region)
    let mk_ARROW    region = ARROW    (wrap_arrow    region)
    let mk_NE       region = NE       (wrap_ne       region)
    let mk_SHARP    region = SHARP    (wrap_sharp    region)
    let mk_ASS      region = ASS      (wrap_ass      region)

    (* All symbol smart constructors *)

    let symbols = [
      mk_SEMI;
      mk_COMMA;
      mk_LPAR;
      mk_RPAR;
      mk_LBRACKET;
      mk_RBRACKET;
      mk_LBRACE;
      mk_RBRACE;
      mk_EQ;
      mk_COLON;
      mk_VBAR;
      mk_DOT;
      mk_WILD;
      mk_PLUS;
      mk_MINUS;
      mk_TIMES;
      mk_SLASH;
      mk_LT;
      mk_LE;
      mk_GT;
      mk_GE;

      mk_CARET;
      mk_ARROW;
      mk_NE;
      mk_SHARP;
      mk_ASS
    ]

    (* All symbols *)

    let symbols =
      let add map (key, data) =
        match SMap.add ~key ~data map with
          `Ok map -> map
        | `Duplicate -> map in
      let apply map mk_kwd =
        add map (to_lexeme (mk_kwd Region.ghost), mk_kwd)
      in List.fold_left ~f:apply ~init:SMap.empty symbols

    (* Ghost symbols *)

    let ghost_semi     = wrap_semi     Region.ghost
    let ghost_comma    = wrap_comma    Region.ghost
    let ghost_lpar     = wrap_lpar     Region.ghost
    let ghost_rpar     = wrap_rpar     Region.ghost
    let ghost_lbrace   = wrap_lbrace   Region.ghost
    let ghost_rbrace   = wrap_rbrace   Region.ghost
    let ghost_lbracket = wrap_lbracket Region.ghost
    let ghost_rbracket = wrap_rbracket Region.ghost
    let ghost_sharp    = wrap_sharp    Region.ghost
    let ghost_vbar     = wrap_vbar     Region.ghost
    let ghost_arrow    = wrap_arrow    Region.ghost
    let ghost_ass      = wrap_ass      Region.ghost
    let ghost_eq       = wrap_eq       Region.ghost
    let ghost_colon    = wrap_colon    Region.ghost
    let ghost_lt       = wrap_lt       Region.ghost
    let ghost_le       = wrap_le       Region.ghost
    let ghost_gt       = wrap_gt       Region.ghost
    let ghost_ge       = wrap_ge       Region.ghost
    let ghost_ne       = wrap_ne       Region.ghost
    let ghost_plus     = wrap_plus     Region.ghost
    let ghost_minus    = wrap_minus    Region.ghost
    let ghost_slash    = wrap_slash    Region.ghost
    let ghost_times    = wrap_times    Region.ghost
    let ghost_dot      = wrap_dot      Region.ghost
    let ghost_wild     = wrap_wild     Region.ghost
    let ghost_caret    = wrap_caret    Region.ghost

    let ghost_SEMI     = SEMI     ghost_semi
    let ghost_COMMA    = COMMA    ghost_comma
    let ghost_LPAR     = LPAR     ghost_lpar
    let ghost_RPAR     = RPAR     ghost_rpar
    let ghost_LBRACE   = LBRACE   ghost_lbrace
    let ghost_RBRACE   = RBRACE   ghost_rbrace
    let ghost_LBRACKET = LBRACKET ghost_lbracket
    let ghost_RBRACKET = RBRACKET ghost_rbracket
    let ghost_SHARP    = SHARP    ghost_sharp
    let ghost_VBAR     = VBAR     ghost_vbar
    let ghost_ARROW    = ARROW    ghost_arrow
    let ghost_ASS      = ASS      ghost_ass
    let ghost_EQ       = EQ       ghost_eq
    let ghost_COLON    = COLON    ghost_colon
    let ghost_LT       = LT       ghost_lt
    let ghost_LE       = LE       ghost_le
    let ghost_GT       = GT       ghost_gt
    let ghost_GE       = GE       ghost_ge
    let ghost_NE       = NE       ghost_ne
    let ghost_PLUS     = PLUS     ghost_plus
    let ghost_MINUS    = MINUS    ghost_minus
    let ghost_SLASH    = SLASH    ghost_slash
    let ghost_TIMES    = TIMES    ghost_times
    let ghost_DOT      = DOT      ghost_dot
    let ghost_WILD     = WILD     ghost_wild
    let ghost_CARET    = CARET    ghost_caret


    (* OTHER GHOST TOKENS *)

    (* IMPORTANT: These values cannot be exported in Token.mli *)

    let wrap_string   s = Wrap.wrap s
    let wrap_verbatim s = Wrap.wrap s
    let wrap_bytes    b = Wrap.wrap ("0x" ^ Hex.show b, b)
    let wrap_int      z = Wrap.wrap (Z.to_string z, z)
    let wrap_nat      z = Wrap.wrap (Z.to_string z ^ "n", z)
    let wrap_mutez    i = Wrap.wrap (Int64.to_string i ^ "mutez", i)
    let wrap_ident    i = Wrap.wrap i
    let wrap_uident   c = Wrap.wrap c

    let wrap_attr key value region =
      Region.{value = (key, value); region}

    let wrap_lang lang region =
      let start = region#start#shift_bytes (String.length "[%") in
      let lang_reg = Region.make ~start ~stop:region#stop in
      Region.{region; value = {value=lang; region=lang_reg}}

    let ghost_string   s = wrap_string   s   Region.ghost
    let ghost_verbatim s = wrap_verbatim s   Region.ghost
    let ghost_bytes    b = wrap_bytes    b   Region.ghost
    let ghost_int      z = wrap_int      z   Region.ghost
    let ghost_nat      z = wrap_nat      z   Region.ghost
    let ghost_mutez    i = wrap_mutez    i   Region.ghost
    let ghost_ident    i = wrap_ident    i   Region.ghost
    let ghost_uident   c = wrap_uident   c   Region.ghost
    let ghost_attr   k v = wrap_attr     k v Region.ghost
    let ghost_lang     l = wrap_lang     l   Region.ghost

    let ghost_String   s = String   (ghost_string s)
    let ghost_Verbatim s = Verbatim (ghost_verbatim s)
    let ghost_Bytes    b = Bytes    (ghost_bytes b)
    let ghost_Int      z = Int      (ghost_int z)
    let ghost_Nat      z = Nat      (ghost_nat z)
    let ghost_Mutez    i = Mutez    (ghost_mutez i)
    let ghost_Ident    i = Ident    (ghost_ident i)
    let ghost_UIdent   c = UIdent   (ghost_uident c)
    let ghost_Attr   k v = Attr     (ghost_attr k v)
    let ghost_Lang     l = Lang     (ghost_lang l)

    (* END-OF-FILE TOKEN *)

    let wrap_eof      = wrap ""
    let mk_EOF region = EOF (wrap_eof region)
    let ghost_eof     = wrap_eof Region.ghost
    let ghost_EOF     = mk_EOF Region.ghost


    (* FROM TOKEN STRINGS TO LEXEMES *)

    let concrete = function
      (* Literals *)

      "Ident"    -> "x"
    | "UIdent"   -> "C"
    | "Int"      -> "1"
    | "Nat"      -> "1n"
    | "Mutez"    -> "1mutez"
    | "String"   -> "\"a string\""
    | "Verbatim" -> "{|verbatim|}"
    | "Bytes"    -> "0xAA"
    | "Attr"     -> "[@attr]"
    | "Lang"     -> "[%Michelson"

    (* Symbols *)

    | "SEMI"     -> ghost_semi#payload
    | "COMMA"    -> ghost_comma#payload
    | "LPAR"     -> ghost_lpar#payload
    | "RPAR"     -> ghost_rpar#payload
    | "LBRACE"   -> ghost_lbrace#payload
    | "RBRACE"   -> ghost_rbrace#payload
    | "LBRACKET" -> ghost_lbracket#payload
    | "RBRACKET" -> ghost_rbracket#payload
    | "SHARP"    -> ghost_sharp#payload
    | "VBAR"     -> ghost_vbar#payload
    | "ARROW"    -> ghost_arrow#payload
    | "ASS"      -> ghost_ass#payload
    | "EQ"       -> ghost_eq#payload
    | "COLON"    -> ghost_colon#payload
    | "LT"       -> ghost_lt#payload
    | "LE"       -> ghost_le#payload
    | "GT"       -> ghost_gt#payload
    | "GE"       -> ghost_ge#payload
    | "NE"       -> ghost_ne#payload
    | "PLUS"     -> ghost_plus#payload
    | "MINUS"    -> ghost_minus#payload
    | "SLASH"    -> ghost_slash#payload
    | "TIMES"    -> ghost_times#payload
    | "DOT"      -> ghost_dot#payload
    | "WILD"     -> ghost_wild#payload
    | "CARET"    -> ghost_caret#payload

    (* Keywords *)

    | "And"       -> ghost_and#payload
    | "Begin"     -> ghost_begin#payload
    | "BigMap"    -> ghost_big_map#payload
    | "Block"     -> ghost_block#payload
    | "Case"      -> ghost_case#payload
    | "Const"     -> ghost_const#payload
    | "Contains"  -> ghost_contains#payload
    | "Else"      -> ghost_else#payload
    | "End"       -> ghost_end#payload
    | "For"       -> ghost_for#payload
    | "From"      -> ghost_from#payload
    | "Function"  -> ghost_function#payload
    | "If"        -> ghost_if#payload
    | "In"        -> ghost_in#payload
    | "Is"        -> ghost_is#payload
    | "List"      -> ghost_list#payload
    | "Map"       -> ghost_map#payload
    | "Mod"       -> ghost_mod#payload
    | "Module"    -> ghost_module#payload
    | "Nil"       -> ghost_nil#payload
    | "Not"       -> ghost_not#payload
    | "Of"        -> ghost_of#payload
    | "Or"        -> ghost_or#payload
    | "Patch"     -> ghost_patch#payload
    | "Record"    -> ghost_record#payload
    | "Recursive" -> ghost_recursive#payload
    | "Remove"    -> ghost_remove#payload
    | "Set"       -> ghost_set#payload
    | "Skip"      -> ghost_skip#payload
    | "Step"      -> ghost_step#payload
    | "Then"      -> ghost_then#payload
    | "To"        -> ghost_to#payload
    | "Type"      -> ghost_type#payload
    | "Var"       -> ghost_var#payload
    | "While"     -> ghost_while#payload
    | "With"      -> ghost_with#payload

    (* End-Of-File *)

    | "EOF" -> ghost_eof#payload

    (* This case should not happen! *)

    | _  -> "\\Unknown" (* Backslash meant to trigger an error *)


    (* FROM TOKENS TO TOKEN STRINGS AND REGIONS *)

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
        let s, b = t#payload in
        t#region,
        sprintf "Bytes (%S, \"0x%s\")" s (Hex.show b)
    | Int t ->
        let s, n = t#payload in
        t#region, sprintf "Int (%S, %s)" s (Z.to_string n)
    | Nat t ->
        let s, n = t#payload in
        t#region, sprintf "Nat (%S, %s)" s (Z.to_string n)
    | Mutez t ->
        let s, n = t#payload in
        t#region, sprintf "Mutez (%S, %s)" s (Int64.to_string n)
    | Ident t ->
        t#region, sprintf "Ident %S" t#payload
    | UIdent t ->
        t#region, sprintf "UIdent %S" t#payload
    | Attr {region; value} ->
        region, sprintf "Attr %s" (Attr.to_string value)
    | Lang {value = {value = payload; _}; region; _} ->
        region, sprintf "Lang %S" payload

    (* Symbols *)

    | SEMI     t -> t#region, "SEMI"
    | COMMA    t -> t#region, "COMMA"
    | LPAR     t -> t#region, "LPAR"
    | RPAR     t -> t#region, "RPAR"
    | LBRACE   t -> t#region, "LBRACE"
    | RBRACE   t -> t#region, "RBRACE"
    | LBRACKET t -> t#region, "LBRACKET"
    | RBRACKET t -> t#region, "RBRACKET"
    | SHARP    t -> t#region, "SHARP"
    | VBAR     t -> t#region, "VBAR"
    | ARROW    t -> t#region, "ARROW"
    | ASS      t -> t#region, "ASS"
    | EQ       t -> t#region, "EQ"
    | COLON    t -> t#region, "COLON"
    | LT       t -> t#region, "LT"
    | LE       t -> t#region, "LE"
    | GT       t -> t#region, "GT"
    | GE       t -> t#region, "GE"
    | NE       t -> t#region, "NE"
    | PLUS     t -> t#region, "PLUS"
    | MINUS    t -> t#region, "MINUS"
    | SLASH    t -> t#region, "SLASH"
    | TIMES    t -> t#region, "TIMES"
    | DOT      t -> t#region, "DOT"
    | WILD     t -> t#region, "WILD"
    | CARET    t -> t#region, "CARET"

    (* Keywords *)

    | And       t -> t#region, "And"
    | Begin     t -> t#region, "Begin"
    | BigMap    t -> t#region, "BigMap"
    | Block     t -> t#region, "Block"
    | Case      t -> t#region, "Case"
    | Const     t -> t#region, "Const"
    | Contains  t -> t#region, "Contains"
    | Else      t -> t#region, "Else"
    | End       t -> t#region, "End"
    | For       t -> t#region, "For"
    | From      t -> t#region, "From"
    | Function  t -> t#region, "Function"
    | If        t -> t#region, "If"
    | In        t -> t#region, "In"
    | Is        t -> t#region, "Is"
    | List      t -> t#region, "List"
    | Map       t -> t#region, "Map"
    | Mod       t -> t#region, "Mod"
    | Module    t -> t#region, "Module"
    | Nil       t -> t#region, "Nil"
    | Not       t -> t#region, "Not"
    | Of        t -> t#region, "Of"
    | Or        t -> t#region, "Or"
    | Patch     t -> t#region, "Patch"
    | Record    t -> t#region, "Record"
    | Recursive t -> t#region, "Recursive"
    | Remove    t -> t#region, "Remove"
    | Set       t -> t#region, "Set"
    | Skip      t -> t#region, "Skip"
    | Step      t -> t#region, "Step"
    | Then      t -> t#region, "Then"
    | To        t -> t#region, "To"
    | Type      t -> t#region, "Type"
    | Var       t -> t#region, "Var"
    | While     t -> t#region, "While"
    | With      t -> t#region, "With"

    (* End-Of-File *)

    | EOF t -> t#region, "EOF"


    (* CONVERSIONS *)

    let to_string ~offsets mode token =
      let region, val_str = proj_token token in
      let reg_str = region#compact ~offsets mode
      in sprintf "%s: %s" reg_str val_str

    let to_region token = proj_token token |> fst


    (* EXPORTED SMART CONSTRUCTORS *)

    (* Keywords *)

    type kwd_err = Invalid_keyword

    let mk_kwd ident region =
      match SMap.find keywords ident with
        Some mk_kwd -> Ok (mk_kwd region)
      |        None -> Error Invalid_keyword

    (* Directives *)

    let mk_directive dir = Directive dir

    (* Strings *)

    let mk_string lexeme region = String (wrap lexeme region)

    (* Verbatim strings *)

    let mk_verbatim lexeme region = Verbatim (wrap lexeme region)

    (* Bytes *)

    let mk_bytes lexeme bytes region =
      Bytes (wrap (lexeme, `Hex bytes) region)

    (* Integers *)

    let mk_int lexeme z region = Int (wrap (lexeme, z) region)

    (* Natural numbers *)

    type nat_err = Wrong_nat_syntax of string (* Not PascaLIGO *)

    let mk_nat nat z region = Ok (Nat (wrap (nat ^ "n", z) region))

    (* Mutez *)

    type mutez_err = Wrong_mutez_syntax of string (* Not PascaLIGO *)

    let mk_mutez nat ~suffix int64 region =
      Ok (Mutez (wrap (nat ^ suffix, int64) region))

    (* End-Of-File *)

    let mk_eof region = EOF (wrap "" region)

    (* Symbols *)

    type sym_err = Invalid_symbol of string

    let mk_sym lexeme region =
      match SMap.find symbols lexeme with
        Some mk_sym -> Ok (mk_sym region)
      |        None -> Error (Invalid_symbol lexeme)

    (* Identifiers *)

    let mk_ident value region =
      match SMap.find keywords value with
        Some mk_kwd -> mk_kwd region
      |        None -> Ident (wrap value region)

    (* Constructors/Modules *)

    let mk_uident value region = UIdent (wrap value region)

    (* Attributes *)

    let mk_attr ~key ?value region = Attr {region; value = key, value}

    (* Code injection *)

    type lang_err = Wrong_lang_syntax of string (* Not PascaLIGO *)

    let mk_lang lang region = Ok (Lang Region.{value=lang; region})

    (* PREDICATES *)

    let is_int    = function Int    _ -> true | _ -> false
    let is_string = function String _ -> true | _ -> false
    let is_bytes  = function Bytes  _ -> true | _ -> false
    let is_eof    = function EOF    _ -> true | _ -> false

    let hex_digits = ["A"; "B"; "C"; "D"; "E"; "F";
                      "a"; "b"; "c"; "d"; "e"; "f"]

    let is_hex = function
      UIdent t | Ident t ->
        List.mem hex_digits t#payload ~equal:String.equal
    | _ -> false

    let is_sym = function
      SEMI _
    | COMMA _
    | LPAR _
    | RPAR _
    | LBRACE _
    | RBRACE _
    | LBRACKET _
    | RBRACKET _
    | SHARP _
    | VBAR _
    | ARROW _
    | ASS _
    | EQ _
    | COLON _
    | LT _
    | LE _
    | GT _
    | GE _
    | NE _
    | PLUS _
    | MINUS _
    | SLASH _
    | TIMES _
    | DOT _
    | WILD _
    | CARET _ -> true
    | _ -> false

    (* String delimiters *)

    let support_string_delimiter c = Char.(c = '"')

    (* Verbatim strings *)

    let verbatim_delimiters = ("{|", "|}")
  end

include T

module type S = module type of T
