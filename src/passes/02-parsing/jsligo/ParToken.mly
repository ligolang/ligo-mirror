(* Tokens (mirroring these defined in module Token) *)

(* All external symbols here should be unqualified because this file
   is used by [menhir] that does not always insert the [%{..%}]
   header. So we work around it by the [-open Module] option in [dune]
   but symbols should be unqualified.

   Also, keep in mind that [ParToken.mly] and [Parser.mly] are merged
   into one file, and the header of [Parser.mly] affects this code. *)

%[@recover.prelude
  (* See [dune] file for [-open] flags for modules used in the
     semantic value of tokens, like [Wrap]. *)

  module Directive = LexerLib.Directive
  module Region    = Simple_utils.Region
  module Token     = Lexing_jsligo.Token

  (* Ghosty semantic values for inserted tokens *)


  let ghost_block_com  = Token.ghost_block_com
  let ghost_line_com   = Token.ghost_line_com
  let ghost_linemarker = Directive.ghost_Linemarker
  let ghost_string     = Token.ghost_string   "ghost string"
  let ghost_verbatim   = Token.ghost_verbatim "ghost verbatim"
  let ghost_bytes      = Token.ghost_bytes    (Hex.of_string "Ghost bytes")
  let ghost_int        = Token.ghost_int      Z.zero
(*
  let ghost_nat        = Token.ghost_nat      Z.zero
  let ghost_mutez      = Token.ghost_mutez    Int64.zero
*)
  let ghost_ident      = Token.ghost_ident    "ghost_ident"
  let ghost_uident     = Token.ghost_uident   "Ghost_uident"
  let ghost_attr       = Token.ghost_attr     "ghost_attr"
(*
  let ghost_lang       = Token.ghost_lang     "Ghost_lang"
*)
 ]

(* Literals *)

%token             <string Wrap.wrap> BlockCom  "<block_comment>" [@recover.expr ghost_block_com]
%token             <string Wrap.wrap> LineCom   "<line_comment>"  [@recover.expr ghost_line_com]

%token         <LexerLib.Directive.t> Directive "<directive>" [@recover.expr ghost_linemarker]
%token                <string Wrap.t> String    "<string>"    [@recover.expr ghost_string]
%token                <string Wrap.t> Verbatim  "<verbatim>"  [@recover.expr ghost_verbatim]
%token      <(string * Hex.t) Wrap.t> Bytes     "<bytes>"     [@recover.expr ghost_bytes]
%token        <(string * Z.t) Wrap.t> Int       "<int>"       [@recover.expr ghost_int]
(*
%token        <(string * Z.t) Wrap.t> Nat       "<nat>"       [@recover.expr ghost_nat]
%token    <(string * Int64.t) Wrap.t> Mutez     "<mutez>"     [@recover.expr ghost_mutez]
*)
%token                <string Wrap.t> Ident     "<ident>"     [@recover.expr ghost_ident]
%token                <string Wrap.t> UIdent    "<uident>"    [@recover.expr ghost_uident]
%token            <Attr.t Region.reg> Attr      "[@attr]"     [@recover.expr ghost_attr]
(*
%token <string Region.reg Region.reg> Lang      "[%lang"      [@recover.expr ghost_lang]
*)

(* Symbols *)

%token <string Wrap.wrap> MINUS   "-"    [@recover.expr Token.ghost_minus]
%token <string Wrap.wrap> PLUS    "+"    [@recover.expr Token.ghost_plus]
%token <string Wrap.wrap> SLASH   "/"    [@recover.expr Token.ghost_slash]
%token <string Wrap.wrap> TIMES   "*"    [@recover.expr Token.ghost_times]
%token <string Wrap.wrap> REM     "%"    [@recover.expr Token.ghost_rem]
(*
%token <string Wrap.wrap> PLUS2   "++"   [@recover.expr Token.ghost_plus2]
%token <string Wrap.wrap> MINUS2  "--"   [@recover.expr Token.ghost_minus2]
*)
%token <string Wrap.wrap> LPAR     "("   [@recover.expr Token.ghost_lpar]
%token <string Wrap.wrap> RPAR     ")"   [@recover.expr Token.ghost_rpar]
%token <string Wrap.wrap> LBRACKET "["   [@recover.expr Token.ghost_lbracket]
%token <string Wrap.wrap> RBRACKET "]"   [@recover.expr Token.ghost_rbracket]
%token <string Wrap.wrap> LBRACE   "{"   [@recover.expr Token.ghost_lbrace]
%token <string Wrap.wrap> RBRACE   "}"   [@recover.expr Token.ghost_rbrace]
%token <string Wrap.wrap> COMMA    ","   [@recover.expr Token.ghost_comma]
%token <string Wrap.wrap> SEMI     ";"   [@recover.expr Token.ghost_semi]
%token <string Wrap.wrap> COLON    ":"   [@recover.expr Token.ghost_colon]
%token <string Wrap.wrap> DOT      "."   [@recover.expr Token.ghost_dot]
%token <string Wrap.wrap> ELLIPSIS "..." [@recover.expr Token.ghost_ellipsis]
%token <string Wrap.wrap> BOOL_OR  "||"  [@recover.expr Token.ghost_bool_or]
%token <string Wrap.wrap> BOOL_AND "&&"  [@recover.expr Token.ghost_bool_and]
%token <string Wrap.wrap> BOOL_NOT "!"   [@recover.expr Token.ghost_bool_not]
(*
%token <string Wrap.wrap> BIT_AND  "&"   [@recover.expr Token.ghost_bit_and]
%token <string Wrap.wrap> BIT_NOT  "~"   [@recover.expr Token.ghost_bit_not]
%token <string Wrap.wrap> BIT_XOR  "^"   [@recover.expr Token.ghost_bit_xor]
%token <string Wrap.wrap> SHIFT_L  "<<<" [@recover.expr Token.ghost_shift_l]
%token <string Wrap.wrap> SHIFT_R  ">>>" [@recover.expr Token.ghost_shift_r]
*)
%token <string Wrap.wrap> EQ       "="   [@recover.expr Token.ghost_eq]
%token <string Wrap.wrap> EQ2      "=="  [@recover.expr Token.ghost_eq2]
%token <string Wrap.wrap> NE       "!="  [@recover.expr Token.ghost_ne]
%token <string Wrap.wrap> LT       "<"   [@recover.expr Token.ghost_lt]
%token <string Wrap.wrap> GT       ">"   [@recover.expr Token.ghost_gt]
%token <string Wrap.wrap> LE       "<="  [@recover.expr Token.ghost_le]
%token <string Wrap.wrap> GE       ">="  [@recover.expr Token.ghost_ge]
%token <string Wrap.wrap> PLUS_EQ  "+="  [@recover.expr Token.ghost_plus_eq]
%token <string Wrap.wrap> MINUS_EQ "-="  [@recover.expr Token.ghost_minus_eq]
%token <string Wrap.wrap> MULT_EQ  "*="  [@recover.expr Token.ghost_mult_eq]
%token <string Wrap.wrap> REM_EQ   "%="  [@recover.expr Token.ghost_rem_eq]
%token <string Wrap.wrap> DIV_EQ   "/="  [@recover.expr Token.ghost_div_eq]
(*
%token <string Wrap.wrap> SL_EQ    "<<<=" [@recover.expr Token.ghost_sl_eq]
%token <string Wrap.wrap> SR_EQ    ">>>=" [@recover.expr Token.ghost_sr_eq]
%token <string Wrap.wrap> AND_EQ   "&="   [@recover.expr Token.ghost_and_eq]
%token <string Wrap.wrap> OR_EQ    "|="   [@recover.expr Token.ghost_or_eq]
%token <string Wrap.wrap> XOR_EQ   "^="   [@recover.expr Token.ghost_xor_eq]
*)
%token <string Wrap.wrap> VBAR     "|"   [@recover.expr Token.ghost_vbar]
%token <string Wrap.wrap> ARROW    "=>"  [@recover.expr Token.ghost_arrow]
%token <string Wrap.wrap> WILD     "_"   [@recover.expr Token.ghost_wild]

(* JavaScript Keywords *)

%token <string Wrap.wrap> Break   "break"   [@recover.expr Token.ghost_break]
%token <string Wrap.wrap> Case    "case"    [@recover.expr Token.ghost_case]
%token <string Wrap.wrap> Const   "const"   [@recover.expr Token.ghost_const]
%token <string Wrap.wrap> Default "default" [@recover.expr Token.ghost_default]
%token <string Wrap.wrap> Else    "else"    [@recover.expr Token.ghost_else]
%token <string Wrap.wrap> Export  "export"  [@recover.expr Token.ghost_export]
%token <string Wrap.wrap> For     "for"     [@recover.expr Token.ghost_for]
%token <string Wrap.wrap> If      "if"      [@recover.expr Token.ghost_if]
%token <string Wrap.wrap> Import  "import"  [@recover.expr Token.ghost_import]
%token <string Wrap.wrap> Let     "let"     [@recover.expr Token.ghost_let]
%token <string Wrap.wrap> Of      "of"      [@recover.expr Token.ghost_of]
%token <string Wrap.wrap> Return  "return"  [@recover.expr Token.ghost_return]
%token <string Wrap.wrap> Switch  "switch"  [@recover.expr Token.ghost_switch]
%token <string Wrap.wrap> While   "while"   [@recover.expr Token.ghost_while]

(* TypeScript keywords *)

%token <string Wrap.wrap> As        "as"    [@recover.expr Token.ghost_as]
%token <string Wrap.wrap> Namespace "namespace" [@recover.expr Token.ghost_namespace]
%token <string Wrap.wrap> Type      "type"  [@recover.expr Token.ghost_type]

(* Virtual tokens *)

%token <Region.t> ZWSP [@recover.expr Token.ghost_zwsp]

(* End of File *)

%token <string Wrap.wrap> EOF [@recover.expr Token.ghost_eof]

%%
