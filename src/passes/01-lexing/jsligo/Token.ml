(* Token specification for JsLIGO *)

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

    include Menhir_jsligo_tokens.MenhirToken

    (* TOKENS *)

    type t =
      (* Preprocessing directives *)

      Directive of Directive.t

      (* Comments *)

    | BlockCom of lexeme Wrap.t
    | LineCom  of lexeme Wrap.t

      (* Literals *)

    | String   of lexeme Wrap.t
    | Verbatim of lexeme Wrap.t
    | Bytes    of (lexeme * Hex.t) Wrap.t
    | Int      of (lexeme * Z.t) Wrap.t
 (* | Nat      of (lexeme * Z.t) Wrap.t
    | Mutez    of (lexeme * Int64.t) Wrap.t *)
    | Ident    of lexeme Wrap.t
    | UIdent   of lexeme Wrap.t
 (* | Lang     of lexeme Region.reg *)
    | Attr     of Attr.t Region.reg

    (* Symbols *)

    | MINUS    of lexeme Wrap.t  (* -    *)
    | PLUS     of lexeme Wrap.t  (* +    *)
    | SLASH    of lexeme Wrap.t  (* /    *)
    | TIMES    of lexeme Wrap.t  (* *    *)
    | REM      of lexeme Wrap.t  (* %    *)
 (* | PLUS2    of lexeme Wrap.t  (* ++   *)
    | MINUS2   of lexeme Wrap.t  (* --   *) *)
    | LPAR     of lexeme Wrap.t  (* (    *)
    | RPAR     of lexeme Wrap.t  (* )    *)
    | LBRACKET of lexeme Wrap.t  (* [    *)
    | RBRACKET of lexeme Wrap.t  (* ]    *)
    | LBRACE   of lexeme Wrap.t  (* {    *)
    | RBRACE   of lexeme Wrap.t  (* }    *)
    | COMMA    of lexeme Wrap.t  (* ,    *)
    | SEMI     of lexeme Wrap.t  (* ;    *)
    | COLON    of lexeme Wrap.t  (* :    *)
    | DOT      of lexeme Wrap.t  (* .    *)
    | ELLIPSIS of lexeme Wrap.t  (* ...  *)
    | BOOL_OR  of lexeme Wrap.t  (* ||   *)
    | BOOL_AND of lexeme Wrap.t  (* &&   *)
    | BOOL_NOT of lexeme Wrap.t  (* !    *)
 (* | BIT_AND  of lexeme Wrap.t  (* &    *)
    | BIT_NOT  of lexeme Wrap.t  (* ~    *)
    | BIT_XOR  of lexeme Wrap.t  (* ^    *)
    | SHIFT_L  of lexeme Wrap.t  (* <<<  *)
    | SHIFT_R  of lexeme Wrap.t  (* >>>  *) *)
    | EQ       of lexeme Wrap.t  (* =    *)
    | EQ2      of lexeme Wrap.t  (* ==   *)
    | NE       of lexeme Wrap.t  (* !=   *)
    | LT       of lexeme Wrap.t  (* <    *)
    | GT       of lexeme Wrap.t  (* >    *)
    | LE       of lexeme Wrap.t  (* <=   *)
    | GE       of lexeme Wrap.t  (* >=   *)
    | PLUS_EQ  of lexeme Wrap.t  (* +=   *)
    | MINUS_EQ of lexeme Wrap.t  (* -=   *)
    | MULT_EQ  of lexeme Wrap.t  (* *=   *)
    | REM_EQ   of lexeme Wrap.t  (* %=   *)
    | DIV_EQ   of lexeme Wrap.t  (* /=   *)
 (* | SL_EQ    of lexeme Wrap.t  (* <<<= *)
    | SR_EQ    of lexeme Wrap.t  (* >>>= *)
    | AND_EQ   of lexeme Wrap.t  (* &=   *)
    | OR_EQ    of lexeme Wrap.t  (* |=   *)
    | XOR_EQ   of lexeme Wrap.t  (* ^=   *) *)
    | VBAR     of lexeme Wrap.t  (* |    *)
    | ARROW    of lexeme Wrap.t  (* =>   *)
    | WILD     of lexeme Wrap.t  (* _    *)

    (* JavaScript Keywords *)

    | Break    of lexeme Wrap.t  (* break   *)
    | Case     of lexeme Wrap.t  (* case    *)
 (* | Class    of lexeme Wrap.t  (* class   *) *)
    | Const    of lexeme Wrap.t  (* const   *)
    | Default  of lexeme Wrap.t  (* default *)
    | Else     of lexeme Wrap.t  (* else    *)
    | Export   of lexeme Wrap.t  (* export  *)
    | For      of lexeme Wrap.t  (* for     *)
    | If       of lexeme Wrap.t  (* if      *)
    | Import   of lexeme Wrap.t  (* import  *)
    | Let      of lexeme Wrap.t  (* let     *)
    | Of       of lexeme Wrap.t  (* of      *)
    | Return   of lexeme Wrap.t  (* return  *)
    | Switch   of lexeme Wrap.t  (* switch  *)
 (* | This     of lexeme Wrap.t  (* this    *)
    | Void     of lexeme Wrap.t  (* void    *) *)
    | While    of lexeme Wrap.t  (* while   *)
 (* | With     of lexeme Wrap.t  (* with    *) *)

    (* TypeScript keywords *)

    | As          of lexeme Wrap.t  (* as        *)
    | Namespace   of lexeme Wrap.t  (* namespace *)
    | Type        of lexeme Wrap.t  (* type      *)

    (* Virtual tokens *)

    | ZWSP of Region.t  (* Zero-Width SPace *)

    (* End-Of-File *)

    | EOF of lexeme Wrap.t


    type token = t


    (* FROM TOKENS TO LEXEMES *)

    let to_lexeme = function
      (* Directives *)

      Directive d -> Directive.to_lexeme d

      (* Comments *)

    | LineCom t  -> sprintf "// %s" t#payload
    | BlockCom t -> sprintf "/* %s */" t#payload

      (* Literals *)

    | String t   -> sprintf "%S" t#payload  (* Escaped *)
    | Verbatim t -> String.escaped t#payload
    | Bytes t    -> fst t#payload
    | Int t      -> fst t#payload
    | Ident t
    | UIdent t   -> t#payload
    | Attr t     -> Attr.to_lexeme t.Region.value
 (* | Lang lang  -> "[%" ^ Region.(lang.value.value) *)

    (* Symbols *)

    | MINUS    t
    | PLUS     t
    | SLASH    t
    | TIMES    t
    | REM      t
 (* | PLUS2    t
    | MINUS2   t *)
    | LPAR     t
    | RPAR     t
    | LBRACE   t
    | RBRACE   t
    | LBRACKET t
    | RBRACKET t
    | COMMA    t
    | SEMI     t
    | COLON    t
    | DOT      t
    | ELLIPSIS t
    | BOOL_OR  t
    | BOOL_AND t
    | BOOL_NOT t
 (* | BIT_AND  t
    | BIT_NOT  t
    | BIT_XOR  t
    | SHIFT_L  t
    | SHIFT_R  t *)
    | EQ       t
    | EQ2      t
    | NE       t
    | LT       t
    | GT       t
    | LE       t
    | GE       t
    | PLUS_EQ  t
    | MINUS_EQ t
    | MULT_EQ  t
    | REM_EQ   t
    | DIV_EQ   t
 (* | SL_EQ    t
    | SR_EQ    t
    | AND_EQ   t
    | OR_EQ    t
    | XOR_EQ   t *)
    | VBAR     t
    | ARROW    t
    | WILD     t

    (* JavaScript Keywords *)

    | Break    t
    | Case     t
 (* | Class    t *)
    | Const    t
    | Default  t
    | Else     t
    | Export   t
    | For      t
    | If       t
    | Import   t
    | Let      t
    | Of       t
    | Return   t
    | Switch   t
 (* | This     t
    | Void     t *)
    | While    t
 (* | With     t *)

    (* TypeScript keywords *)

    | As        t
    | Namespace t
    | Type      t -> t#payload

    (* Virtual tokens *)

    | ZWSP _ -> ""

    (* End-Of-File *)

    | EOF _ -> ""


    (* KEYWORDS *)

    (* JavaScript Keywords *)

     let wrap_break   = wrap "break"
     let wrap_case    = wrap "case"
  (* let wrap_class   = wrap "class"   *)
     let wrap_const   = wrap "const"
     let wrap_default = wrap "default"
     let wrap_else    = wrap "else"
     let wrap_export  = wrap "export"
     let wrap_for     = wrap "for"
     let wrap_if      = wrap "if"
     let wrap_import  = wrap "import"
     let wrap_let     = wrap "let"
     let wrap_of      = wrap "of"
     let wrap_return  = wrap "return"
     let wrap_switch  = wrap "switch"
  (* let wrap_this    = wrap "this"
     let wrap_void    = wrap "void"    *)
     let wrap_while   = wrap "while"
  (* let wrap_with    = wrap "with"    *)

     let mk_Break   region = Break   (wrap_break   region)
     let mk_Case    region = Case    (wrap_case    region)
  (* let mk_Class   region = Class   (wrap_class   region) *)
     let mk_Const   region = Const   (wrap_const   region)
     let mk_Default region = Default (wrap_default region)
     let mk_Else    region = Else    (wrap_else    region)
     let mk_Export  region = Export  (wrap_export  region)
     let mk_For     region = For     (wrap_for     region)
     let mk_If      region = If      (wrap_if      region)
     let mk_Import  region = Import  (wrap_import  region)
     let mk_Let     region = Let     (wrap_let     region)
     let mk_Of      region = Of      (wrap_of      region)
     let mk_Return  region = Return  (wrap_return  region)
     let mk_Switch  region = Switch  (wrap_switch  region)
  (* let mk_This    region = This    (wrap_this    region)
     let mk_Void    region = Void    (wrap_void    region) *)
     let mk_While   region = While   (wrap_while   region)
  (* let mk_With    region = With    (wrap_with    region) *)

     (* TypeScript keywords *)

     let wrap_as        = wrap "as"
     let wrap_namespace = wrap "namespace"
     let wrap_type      = wrap "type"

     let mk_As        region = As        (wrap_as        region)
     let mk_Namespace region = Namespace (wrap_namespace region)
     let mk_Type      region = Type      (wrap_type      region)

    (* All keyword smart constructors *)

     let keywords = [
       mk_Break;
       mk_Case;
  (*   mk_Class; *)
       mk_Const;
       mk_Default;
       mk_Else;
       mk_Export;
       mk_For;
       mk_If;
       mk_Import;
       mk_Let;
       mk_Of;
       mk_Return;
       mk_Switch;
  (*   mk_This;
       mk_Void;   *)
       mk_While;
  (*   mk_With;   *)

       mk_As;
       mk_Namespace;
       mk_Type
     ]

    (* All keywords *)

    let keywords =
     let add map (key, value) = SMap.add key value map in
      let apply map mk_kwd =
        add map (to_lexeme (mk_kwd Region.ghost), mk_kwd)
      in List.fold_left apply SMap.empty keywords

    (* Ghost keywords *)

    (* JavaScript Keywords *)

     let ghost_break   = wrap_break   Region.ghost
     let ghost_case    = wrap_case    Region.ghost
  (* let ghost_class   = wrap_class   Region.ghost *)
     let ghost_const   = wrap_const   Region.ghost
     let ghost_default = wrap_default Region.ghost
     let ghost_else    = wrap_else    Region.ghost
     let ghost_export  = wrap_export  Region.ghost
     let ghost_for     = wrap_for     Region.ghost
     let ghost_if      = wrap_if      Region.ghost
     let ghost_import  = wrap_import  Region.ghost
     let ghost_let     = wrap_let     Region.ghost
     let ghost_of      = wrap_of      Region.ghost
     let ghost_return  = wrap_return  Region.ghost
     let ghost_switch  = wrap_switch  Region.ghost
  (* let ghost_this    = wrap_this    Region.ghost
     let ghost_void    = wrap_void    Region.ghost *)
     let ghost_while   = wrap_while   Region.ghost
  (* let ghost_with    = wrap_with    Region.ghost *)

     let ghost_Break   = Break   ghost_break
     let ghost_Case    = Case    ghost_case
  (* let ghost_Class   = Class   ghost_class    *)
     let ghost_Const   = Const   ghost_const
     let ghost_Default = Default ghost_default
     let ghost_Else    = Else    ghost_else
     let ghost_Export  = Export  ghost_export
     let ghost_For     = For     ghost_for
     let ghost_If      = If      ghost_if
     let ghost_Import  = Import  ghost_import
     let ghost_Let     = Let     ghost_let
     let ghost_Of      = Of      ghost_of
     let ghost_Return  = Return  ghost_return
     let ghost_Switch  = Switch  ghost_switch
  (* let ghost_This    = This    ghost_this
     let ghost_Void    = Void    ghost_void     *)
     let ghost_While   = While   ghost_while
  (* let ghost_With    = With    ghost_with     *)

     (* TypeScript keywords *)

     let ghost_as        = wrap_as        Region.ghost
     let ghost_namespace = wrap_namespace Region.ghost
     let ghost_type      = wrap_type      Region.ghost

     let ghost_As        = As        ghost_as
     let ghost_Namespace = Namespace ghost_namespace
     let ghost_Type      = Type      ghost_type


    (* SYMBOLS *)

    let wrap_minus    = wrap "-"
    let wrap_plus     = wrap "+"
    let wrap_slash    = wrap "/"
    let wrap_times    = wrap "*"
    let wrap_rem      = wrap "%"
 (* let wrap_plus2    = wrap "++"
    let wrap_minus2   = wrap "--" *)
    let wrap_lpar     = wrap "("
    let wrap_rpar     = wrap ")"
    let wrap_lbracket = wrap "["
    let wrap_rbracket = wrap "]"
    let wrap_lbrace   = wrap "{"
    let wrap_rbrace   = wrap "}"
    let wrap_comma    = wrap ","
    let wrap_semi     = wrap ";"
    let wrap_colon    = wrap ":"
    let wrap_dot      = wrap "."
    let wrap_ellipsis = wrap "..."
    let wrap_bool_or  = wrap "||"
    let wrap_bool_and = wrap "&&"
    let wrap_bool_not = wrap "!"
 (* let wrap_bit_and  = wrap "&"
    let wrap_bit_not  = wrap "~"
    let wrap_bit_xor  = wrap "^"
    let wrap_shift_l  = wrap "<<<"
    let wrap_shift_r  = wrap ">>>" *)
    let wrap_eq       = wrap "="
    let wrap_eq2      = wrap "=="
    let wrap_ne       = wrap "!="
    let wrap_lt       = wrap "<"
    let wrap_gt       = wrap ">"
    let wrap_le       = wrap "<="
    let wrap_ge       = wrap ">="
    let wrap_plus_eq  = wrap "+="
    let wrap_minus_eq = wrap "-="
    let wrap_mult_eq  = wrap "*="
    let wrap_rem_eq   = wrap "%="
    let wrap_div_eq   = wrap "/="
 (* let wrap_sl_eq    = wrap "<<<="
    let wrap_sr_eq    = wrap ">>>="
    let wrap_and_eq   = wrap "&="
    let wrap_or_eq    = wrap "|="
    let wrap_xor_eq   = wrap "^=" *)
    let wrap_vbar     = wrap "|"
    let wrap_arrow    = wrap "=>"
    let wrap_wild     = wrap "_"

    (* Smart constructors *)

    let mk_MINUS    region = MINUS    (wrap_minus    region)
    let mk_PLUS     region = PLUS     (wrap_plus     region)
    let mk_SLASH    region = SLASH    (wrap_slash    region)
    let mk_TIMES    region = TIMES    (wrap_times    region)
    let mk_REM      region = REM      (wrap_rem      region)
 (* let mk_PLUS2    region = PLUS2    (wrap_plus2    region)
    let mk_MINUS2   region = MINUS2   (wrap_minus2   region) *)
    let mk_LPAR     region = LPAR     (wrap_lpar     region)
    let mk_RPAR     region = RPAR     (wrap_rpar     region)
    let mk_LBRACKET region = LBRACKET (wrap_lbracket region)
    let mk_RBRACKET region = RBRACKET (wrap_rbracket region)
    let mk_LBRACE   region = LBRACE   (wrap_lbrace   region)
    let mk_RBRACE   region = RBRACE   (wrap_rbrace   region)
    let mk_COMMA    region = COMMA    (wrap_comma    region)
    let mk_SEMI     region = SEMI     (wrap_semi     region)
    let mk_COLON    region = COLON    (wrap_colon    region)
    let mk_DOT      region = DOT      (wrap_dot      region)
    let mk_ELLIPSIS region = ELLIPSIS (wrap_ellipsis region)
    let mk_BOOL_OR  region = BOOL_OR  (wrap_bool_or  region)
    let mk_BOOL_AND region = BOOL_AND (wrap_bool_and region)
    let mk_BOOL_NOT region = BOOL_NOT (wrap_bool_not region)
 (* let mk_BIT_AND  region = BIT_AND  (wrap_bit_and  region)
    let mk_BIT_NOT  region = BIT_NOT  (wrap_bit_not  region)
    let mk_BIT_XOR  region = BIT_XOR  (wrap_bit_xor  region)
    let mk_SHIFT_L  region = SHIFT_L  (wrap_shift_l  region)
    let mk_SHIFT_R  region = SHIFT_R  (wrap_shift_r  region) *)
    let mk_EQ       region = EQ       (wrap_eq       region)
    let mk_EQ2      region = EQ2      (wrap_eq2      region)
    let mk_NE       region = NE       (wrap_ne       region)
    let mk_LT       region = LT       (wrap_lt       region)
    let mk_GT       region = GT       (wrap_gt       region)
    let mk_LE       region = LE       (wrap_le       region)
    let mk_GE       region = GE       (wrap_ge       region)
    let mk_PLUS_EQ  region = PLUS_EQ  (wrap_plus_eq  region)
    let mk_MINUS_EQ region = MINUS_EQ (wrap_minus_eq region)
    let mk_MULT_EQ  region = MULT_EQ  (wrap_mult_eq  region)
    let mk_REM_EQ   region = REM_EQ   (wrap_rem_eq   region)
    let mk_DIV_EQ   region = DIV_EQ   (wrap_div_eq   region)
 (* let mk_SL_EQ    region = SL_EQ    (wrap_sl_eq    region)
    let mk_SR_EQ    region = SR_EQ    (wrap_sr_eq    region)
    let mk_AND_EQ   region = AND_EQ   (wrap_and_eq   region)
    let mk_OR_EQ    region = OR_EQ    (wrap_or_eq    region)
    let mk_XOR_EQ   region = XOR_EQ   (wrap_xor_eq   region) *)
    let mk_VBAR     region = VBAR     (wrap_vbar     region)
    let mk_ARROW    region = ARROW    (wrap_arrow    region)
    let mk_WILD     region = WILD     (wrap_wild     region)

    (* All symbol smart constructors *)

    let symbols = [
      mk_MINUS;
      mk_PLUS;
      mk_SLASH;
      mk_TIMES;
      mk_REM;
 (*   mk_PLUS2;
      mk_MINUS2; *)
      mk_LPAR;
      mk_RPAR;
      mk_LBRACKET;
      mk_RBRACKET;
      mk_LBRACE;
      mk_RBRACE;
      mk_COMMA;
      mk_SEMI;
      mk_COLON;
      mk_DOT;
      mk_ELLIPSIS;
      mk_BOOL_OR;
      mk_BOOL_AND;
      mk_BOOL_NOT;
 (*   mk_BIT_AND;
      mk_BIT_NOT;
      mk_BIT_XOR;
      mk_SHIFT_L;
      mk_SHIFT_R;  *)
      mk_EQ;
      mk_EQ2;
      mk_NE;
      mk_LT;
      mk_GT;
      mk_LE;
      mk_GE;
      mk_PLUS_EQ;
      mk_MINUS_EQ;
      mk_MULT_EQ;
      mk_REM_EQ;
      mk_DIV_EQ;
 (*   mk_SL_EQ;
      mk_SR_EQ;
      mk_AND_EQ;
      mk_OR_EQ;
      mk_XOR_EQ;  *)
      mk_VBAR;
      mk_ARROW;
      mk_WILD
    ]

    (* All symbols *)

    let symbols =
      let add map (key, value) = SMap.add key value map in
      let apply map mk_kwd =
        add map (to_lexeme (mk_kwd Region.ghost), mk_kwd)
      in List.fold_left apply SMap.empty symbols

    (* Ghost symbols *)

    let ghost_minus    = wrap_minus    Region.ghost
    let ghost_plus     = wrap_plus     Region.ghost
    let ghost_slash    = wrap_slash    Region.ghost
    let ghost_times    = wrap_times    Region.ghost
    let ghost_rem      = wrap_rem      Region.ghost
 (* let ghost_plus2    = wrap_plus2    Region.ghost
    let ghost_minus2   = wrap_minus2   Region.ghost *)
    let ghost_lpar     = wrap_lpar     Region.ghost
    let ghost_rpar     = wrap_rpar     Region.ghost
    let ghost_lbracket = wrap_lbracket Region.ghost
    let ghost_rbracket = wrap_rbracket Region.ghost
    let ghost_lbrace   = wrap_lbrace   Region.ghost
    let ghost_rbrace   = wrap_rbrace   Region.ghost
    let ghost_comma    = wrap_comma    Region.ghost
    let ghost_semi     = wrap_semi     Region.ghost
    let ghost_colon    = wrap_colon    Region.ghost
    let ghost_dot      = wrap_dot      Region.ghost
    let ghost_ellipsis = wrap_ellipsis Region.ghost
    let ghost_bool_or  = wrap_bool_or  Region.ghost
    let ghost_bool_and = wrap_bool_and Region.ghost
    let ghost_bool_not = wrap_bool_not Region.ghost
 (* let ghost_bit_and  = wrap_bit_and  Region.ghost
    let ghost_bit_not  = wrap_bit_not  Region.ghost
    let ghost_bit_xor  = wrap_bit_xor  Region.ghost
    let ghost_shift_l  = wrap_shift_l  Region.ghost
    let ghost_shift_r  = wrap_shift_r  Region.ghost *)
    let ghost_eq       = wrap_eq       Region.ghost
    let ghost_eq2      = wrap_eq2      Region.ghost
    let ghost_ne       = wrap_ne       Region.ghost
    let ghost_lt       = wrap_lt       Region.ghost
    let ghost_gt       = wrap_gt       Region.ghost
    let ghost_le       = wrap_le       Region.ghost
    let ghost_ge       = wrap_ge       Region.ghost
    let ghost_plus_eq  = wrap_plus_eq  Region.ghost
    let ghost_minus_eq = wrap_minus_eq Region.ghost
    let ghost_mult_eq  = wrap_mult_eq  Region.ghost
    let ghost_rem_eq   = wrap_rem_eq   Region.ghost
    let ghost_div_eq   = wrap_div_eq   Region.ghost
 (* let ghost_sl_eq    = wrap_sl_eq    Region.ghost
    let ghost_sr_eq    = wrap_sr_eq    Region.ghost
    let ghost_and_eq   = wrap_and_eq   Region.ghost
    let ghost_or_eq    = wrap_or_eq    Region.ghost
    let ghost_xor_eq   = wrap_xor_eq   Region.ghost *)
    let ghost_vbar     = wrap_vbar     Region.ghost
    let ghost_arrow    = wrap_arrow    Region.ghost
    let ghost_wild     = wrap_wild     Region.ghost

    let ghost_MINUS    = MINUS    ghost_minus
    let ghost_PLUS     = PLUS     ghost_plus
    let ghost_SLASH    = SLASH    ghost_slash
    let ghost_TIMES    = TIMES    ghost_times
    let ghost_REM      = REM      ghost_rem
 (* let ghost_PLUS2    = PLUS2    ghost_plus2
    let ghost_MINUS2   = MINUS2   ghost_minus *)
    let ghost_LPAR     = LPAR     ghost_lpar
    let ghost_RPAR     = RPAR     ghost_rpar
    let ghost_LBRACKET = LBRACKET ghost_lbracket
    let ghost_RBRACKET = RBRACKET ghost_rbracket
    let ghost_LBRACE   = LBRACE   ghost_lbrace
    let ghost_RBRACE   = RBRACE   ghost_rbrace
    let ghost_COMMA    = COMMA    ghost_comma
    let ghost_SEMI     = SEMI     ghost_semi
    let ghost_COLON    = COLON    ghost_colon
    let ghost_DOT      = DOT      ghost_dot
    let ghost_ELLIPSIS = ELLIPSIS ghost_ellipsis
    let ghost_BOOL_OR  = BOOL_OR  ghost_bool_or
    let ghost_BOOL_AND = BOOL_AND ghost_bool_and
    let ghost_BOOL_NOT = BOOL_NOT ghost_bool_not
 (* let ghost_BIT_AND  = BIT_AND  ghost_bit_and
    let ghost_BIT_NOT  = BIT_NOT  ghost_bit_not
    let ghost_BIT_XOR  = BIT_XOR  ghost_bit_xor
    let ghost_SHIFT_L  = SHIFT_L  ghost_shift_l
    let ghost_SHIFT_R  = SHIFT_R  ghost_shift_r *)
    let ghost_EQ       = EQ       ghost_eq
    let ghost_EQ2      = EQ2      ghost_eq2
    let ghost_NE       = NE       ghost_ne
    let ghost_LT       = LT       ghost_lt
    let ghost_GT       = GT       ghost_gt
    let ghost_LE       = LE       ghost_le
    let ghost_GE       = GE       ghost_ge
    let ghost_PLUS_EQ  = PLUS_EQ  ghost_plus_eq
    let ghost_MINUS_EQ = MINUS_EQ ghost_minus_eq
    let ghost_MULT_EQ  = MULT_EQ  ghost_mult_eq
    let ghost_REM_EQ   = REM_EQ   ghost_rem_eq
    let ghost_DIV_EQ   = DIV_EQ   ghost_div_eq
 (* let ghost_SL_EQ    = SL_EQ    ghost_sl_eq
    let ghost_SR_EQ    = SR_EQ    ghost_sr_eq
    let ghost_AND_EQ   = AND_EQ   ghost_and_eq
    let ghost_OR_EQ    = OR_EQ    ghost_or_eq
    let ghost_XOR_EQ   = XOR_EQ   ghost_xor_eq *)
    let ghost_VBAR     = VBAR     ghost_vbar
    let ghost_ARROW    = ARROW    ghost_arrow
    let ghost_WILD     = WILD     ghost_wild


    (* OTHER GHOST TOKENS *)

    (* IMPORTANT: These values cannot be exported in Token.mli *)

    let ghost_string   s = Wrap.ghost s
    let ghost_verbatim s = Wrap.ghost s
    let ghost_bytes    b = Wrap.ghost ("0x" ^ Hex.show b, b)
    let ghost_int      z = Wrap.ghost (Z.to_string z, z)
    let ghost_ident    i = Wrap.ghost i
    let ghost_uident   c = Wrap.ghost c
    let ghost_attr     a = Region.(wrap_ghost (a, None))

    let ghost_String   s = String   (ghost_string s)
    let ghost_Verbatim s = Verbatim (ghost_verbatim s)
    let ghost_Bytes    b = Bytes    (ghost_bytes b)
    let ghost_Int      z = Int      (ghost_int z)
    let ghost_Ident    i = Ident    (ghost_ident i)
    let ghost_UIdent   c = UIdent   (ghost_uident c)
    let ghost_Attr     a = Attr     (ghost_attr a)

    (* COMMENTS *)

    let ghost_block_com = Wrap.ghost "(* A block comment *)"
    let ghost_BlockCom  = BlockCom ghost_block_com
    let ghost_line_com  = Wrap.ghost "// A line comment"
    let ghost_LineCom   = LineCom ghost_line_com

    (* VIRTUAL TOKENS *)

    let ghost_zwsp = Region.ghost
    let ghost_ZWSP = ZWSP ghost_zwsp

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
    | "String"   -> "\"a string\""
    | "Verbatim" -> "{|verbatim|}"
    | "Bytes"    -> "0xAA"
    | "Attr"     -> "[@attr]"

    (* Symbols *)

    | "MINUS"    -> ghost_minus#payload
    | "PLUS"     -> ghost_plus#payload
    | "SLASH"    -> ghost_slash#payload
    | "TIMES"    -> ghost_times#payload
    | "REM"      -> ghost_rem#payload
 (* | "PLUS2"    -> ghost_plus2#payload
    | "MINUS2"   -> ghost_minus2#payload *)
    | "LPAR"     -> ghost_lpar#payload
    | "RPAR"     -> ghost_rpar#payload
    | "LBRACE"   -> ghost_lbrace#payload
    | "RBRACE"   -> ghost_rbrace#payload
    | "LBRACKET" -> ghost_lbracket#payload
    | "RBRACKET" -> ghost_rbracket#payload
    | "COMMA"    -> ghost_comma#payload
    | "SEMI"     -> ghost_semi#payload
    | "COLON"    -> ghost_colon#payload
    | "DOT"      -> ghost_dot#payload
    | "ELLIPSIS" -> ghost_ellipsis#payload
    | "BOOL_OR"  -> ghost_bool_or#payload
    | "BOOL_AND" -> ghost_bool_and#payload
    | "BOOL_NOT" -> ghost_bool_not#payload
 (* | "BIT_AND"  -> ghost_and#payload
    | "BIT_NOT"  -> ghost_not#payload
    | "BIT_XOR"  -> ghost_xor#payload
    | "SHIFT_L"  -> ghost_shift_l#payload
    | "SHIFT_R"  -> ghost_shift_r#payload *)
    | "EQ"       -> ghost_eq#payload
    | "EQ2"      -> ghost_eq2#payload
    | "NE"       -> ghost_ne#payload
    | "LT"       -> ghost_lt#payload
    | "GT"       -> ghost_gt#payload
    | "LE"       -> ghost_le#payload
    | "GE"       -> ghost_ge#payload
    | "PLUS_EQ"  -> ghost_plus_eq#payload
    | "MINUS_EQ" -> ghost_minus_eq#payload
    | "MULT_EQ"  -> ghost_mult_eq#payload
    | "REM_EQ"   -> ghost_rem_eq#payload
    | "DIV_EQ"   -> ghost_div_eq#payload
 (* | "SL_EQ"    -> ghost_sl_eq#payload
    | "SR_EQ"    -> ghost_sr_eq#payload
    | "AND_EQ"   -> ghost_and_eq#payload
    | "OR_EQ"    -> ghost_or_eq#payload
    | "XOR_EQ"   -> ghost_xor_eq#payload *)
    | "VBAR"     -> ghost_vbar#payload
    | "ARROW"    -> ghost_arrow#payload
    | "WILD"     -> ghost_wild#payload

    (* JavaScript Keywords *)

 (* | "Break"    -> ghost_break#payload *)
    | "Case"     -> ghost_case#payload
 (* | "Class"    -> ghost_class#payload *)
    | "Const"    -> ghost_const#payload
    | "Default"  -> ghost_default#payload
    | "Else"     -> ghost_else#payload
    | "Export"   -> ghost_export#payload
    | "For"      -> ghost_for#payload
    | "If"       -> ghost_if#payload
    | "Import"   -> ghost_import#payload
    | "Let"      -> ghost_let#payload
    | "Of"       -> ghost_of#payload
    | "Return"   -> ghost_return#payload
    | "Break"    -> ghost_break#payload
    | "Switch"   -> ghost_switch#payload
 (* | "This"     -> ghost_this#payload
    | "Void"     -> ghost_void#payload *)
    | "While"    -> ghost_while#payload
 (* | "With"     -> ghost_with#payload *)

    (* TypeScript keywords *)

    | "Type"      -> ghost_type#payload
    | "Namespace" -> ghost_namespace#payload
    | "As"        -> ghost_as#payload

    (* Virtual tokens *)

    | "ZWSP" -> ""

    (* End-Of-File *)

    | "EOF" -> ""

    (* This case should not happen! *)

    | _  -> "\\Unknown" (* Backslash meant to trigger an error *)


    (* FROM TOKENS TO TOKEN STRINGS AND REGIONS *)

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
        let s, b = t#payload in
        t#region,
        sprintf "Bytes (%S, \"0x%s\")" s (Hex.show b)
    | Int t ->
        let s, n = t#payload in
        t#region, sprintf "Int (%S, %s)" s (Z.to_string n)
 (* | Nat t ->
        let s, n = t#payload in
        t#region, sprintf "Nat (%S, %s)" s (Z.to_string n)
    | Mutez t ->
        let s, n = t#payload in
        t#region, sprintf "Mutez (%S, %s)" s (Int64.to_string n) *)
    | Ident t ->
        t#region, sprintf "Ident %S" t#payload
    | UIdent t ->
        t#region, sprintf "UIdent %S" t#payload
    | Attr {region; value} ->
        region, sprintf "Attr %s" (Attr.to_string value)
 (* | Lang {value = {value = payload; _}; region; _} ->
        region, sprintf "Lang %S" payload *)

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
    | LBRACE   t -> t#region, "LBRACE"
    | RBRACE   t -> t#region, "RBRACE"
    | LBRACKET t -> t#region, "LBRACKET"
    | RBRACKET t -> t#region, "RBRACKET"
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

    | ZWSP region -> region, "ZWSP"

    (* End-Of-File *)

    | EOF t -> t#region, "EOF"


    (* CONVERSIONS *)

    let to_string ~offsets mode token =
      let region, val_str = proj_token token in
      let reg_str = region#compact ~offsets mode
      in sprintf "%s: %s" reg_str val_str

    let to_region token = proj_token token |> fst

    (* SMART CONSTRUCTORS *)

    (* Keywords *)

    type kwd_err = Invalid_keyword

    let mk_kwd ident region =
      match SMap.find_opt ident keywords with
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

    type nat_err = Wrong_nat_syntax of string

    let mk_nat _nat _z _region =
      Error (Wrong_nat_syntax "Example: \"12334 as nat\".")

    (* Mutez *)

    type mutez_err = Wrong_mutez_syntax of string

    let mk_mutez _nat ~suffix:_ _int64 _region =
      Error (Wrong_mutez_syntax "Example: \"1234 as mutez\".")

    (* End-Of-File *)

    let mk_eof region = EOF (wrap "" region)

    (* Symbols *)

    type sym_err = Invalid_symbol of string

    let mk_sym lexeme region =
      match SMap.find_opt lexeme symbols with
        Some mk_sym -> Ok (mk_sym region)
      |        None -> Error (Invalid_symbol lexeme)

    (* Identifiers *)

    let mk_ident value region =
      match SMap.find_opt value keywords with
        Some mk_kwd -> mk_kwd region
      |        None -> Ident (wrap value region)

    (* Constructors/Modules *)

    let mk_uident value region = UIdent (wrap value region)

    (* Attributes *)

    let mk_attr ~key ?value region = Attr {region; value = key, value}

    (* Code injection *)

    type lang_err = Wrong_lang_syntax of string

    let mk_lang _lang _region =
      Error (Wrong_lang_syntax
               "Example: \"(Michelson `{UNPAIR; ADD}`\
                         as ((n: [nat, nat]) => nat))\".")

    (* PREDICATES *)

    let is_int    = function Int    _ -> true | _ -> false
    let is_string = function String _ -> true | _ -> false
    let is_bytes  = function Bytes  _ -> true | _ -> false
    let is_eof    = function EOF    _ -> true | _ -> false

    let hex_digits = ["A"; "B"; "C"; "D"; "E"; "F";
                      "a"; "b"; "c"; "d"; "e"; "f"]

    let is_hex = function
      UIdent t | Ident t -> List.mem t#payload hex_digits
    | _ -> false

    let is_sym = function
      MINUS _
    | PLUS _
    | SLASH _
    | TIMES _
    | REM _
 (* | PLUS2 _
    | MINUS2 _ *)
    | LPAR _
    | RPAR _
    | LBRACKET _
    | RBRACKET _
    | LBRACE _
    | RBRACE _
    | COMMA _
    | SEMI _
    | COLON _
    | DOT _
    | ELLIPSIS _
    | BOOL_OR _
    | BOOL_AND _
    | BOOL_NOT _
 (* | BIT_AND _
    | BIT_NOT _
    | BIT_XOR _
    | SHIFT_L _
    | SHIFT_R _ *)
    | EQ _
    | EQ2 _
    | NE _
    | LT _
    | GT _
    | LE _
    | GE _
 (* | PLUS_EQ _
    | MINUS_EQ _
    | MULT_EQ _
    | REM_EQ _
    | DIV_EQ _
    | SL_EQ _
    | SR_EQ _
    | AND_EQ _
    | OR_EQ _
    | XOR_EQ _ *)
    | VBAR _
    | ARROW _
    | WILD _ -> true
    | _ -> false

    (* String delimiters *)

    let support_string_delimiter c = (c = '"')

    (* Verbatim strings *)

    let verbatim_delimiters = ("{|", "|}")
  end

include T

module type S = module type of T
