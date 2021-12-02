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
  module Token     = Lexing_reasonligo.Token

  (* Ghosty semantic values for inserted tokens *)

  let ghost_linemarker = Directive.ghost_Linemarker
  let ghost_string     = Token.ghost_string   "ghost string"
  let ghost_verbatim   = Token.ghost_verbatim "ghost verbatim"
  let ghost_bytes      = Token.ghost_bytes    (Hex.of_string "Ghost bytes")
  let ghost_int        = Token.ghost_int      Z.zero
  let ghost_nat        = Token.ghost_nat      Z.zero
  let ghost_mutez      = Token.ghost_mutez    Int64.zero
  let ghost_ident      = Token.ghost_ident    "ghost_ident"
  let ghost_uident     = Token.ghost_uident   "Ghost_uident"
  let ghost_attr       = Token.ghost_attr     "ghost_attr"
  let ghost_lang       = Token.ghost_lang     "Ghost_lang"
 ]

(* Literals *)

%token         <LexerLib.Directive.t> Directive "<directive>" [@recover.expr ghost_linemarker]
%token                <string Wrap.t> String    "<string>"    [@recover.expr ghost_string]
%token                <string Wrap.t> Verbatim  "<verbatim>"  [@recover.expr ghost_verbatim]
%token      <(string * Hex.t) Wrap.t> Bytes     "<bytes>"     [@recover.expr ghost_bytes]
%token        <(string * Z.t) Wrap.t> Int       "<int>"       [@recover.expr ghost_int]
%token        <(string * Z.t) Wrap.t> Nat       "<nat>"       [@recover.expr ghost_nat]
%token    <(string * Int64.t) Wrap.t> Mutez     "<mutez>"     [@recover.expr ghost_mutez]
%token                <string Wrap.t> Ident     "<ident>"     [@recover.expr ghost_ident]
%token                <string Wrap.t> UIdent    "<uident>"    [@recover.expr ghost_uident]
%token            <Attr.t Region.reg> Attr      "[@attr]"     [@recover.expr ghost_attr]
%token <string Region.reg Region.reg> Lang      "[%lang"      [@recover.expr ghost_lang]

(* Symbols *)

%token <string Wrap.wrap> MINUS    "-"   [@recover.expr Token.ghost_minus]
%token <string Wrap.wrap> PLUS     "+"   [@recover.expr Token.ghost_plus]
%token <string Wrap.wrap> SLASH    "/"   [@recover.expr Token.ghost_slash]
%token <string Wrap.wrap> TIMES    "*"   [@recover.expr Token.ghost_times]
%token <string Wrap.wrap> LPAR     "("   [@recover.expr Token.ghost_lpar]
%token <string Wrap.wrap> RPAR     ")"   [@recover.expr Token.ghost_rpar]
%token <string Wrap.wrap> LBRACKET "["   [@recover.expr Token.ghost_lbracket]
%token <string Wrap.wrap> RBRACKET "]"   [@recover.expr Token.ghost_rbracket]
%token <string Wrap.wrap> LBRACE   "{"   [@recover.expr Token.ghost_lbrace]
%token <string Wrap.wrap> RBRACE   "}"   [@recover.expr Token.ghost_rbrace]
%token <string Wrap.wrap> PLUS2    "++"  [@recover.expr Token.ghost_plus2]
%token <string Wrap.wrap> DOT      "."   [@recover.expr Token.ghost_dot]
%token <string Wrap.wrap> ELLIPSIS "..." [@recover.expr Token.ghost_ellipsis]
%token <string Wrap.wrap> COMMA    ","   [@recover.expr Token.ghost_comma]
%token <string Wrap.wrap> SEMI     ";"   [@recover.expr Token.ghost_semi]
%token <string Wrap.wrap> COLON    ":"   [@recover.expr Token.ghost_colon]
%token <string Wrap.wrap> VBAR     "|"   [@recover.expr Token.ghost_vbar]
%token <string Wrap.wrap> WILD     "_"   [@recover.expr Token.ghost_wild]
%token <string Wrap.wrap> EQ       "="   [@recover.expr Token.ghost_eq]
%token <string Wrap.wrap> EQ2      "=="  [@recover.expr Token.ghost_eq2]
%token <string Wrap.wrap> NE       "!="  [@recover.expr Token.ghost_ne]
%token <string Wrap.wrap> LT       "<"   [@recover.expr Token.ghost_lt]
%token <string Wrap.wrap> GT       ">"   [@recover.expr Token.ghost_gt]
%token <string Wrap.wrap> LE       "<="  [@recover.expr Token.ghost_le]
%token <string Wrap.wrap> GE       ">="  [@recover.expr Token.ghost_ge]
%token <string Wrap.wrap> ARROW    "=>"  [@recover.expr Token.ghost_arrow]
%token <string Wrap.wrap> NOT      "!"   [@recover.expr Token.ghost_not]
%token <string Wrap.wrap> BOOL_OR  "||"  [@recover.expr Token.ghost_bool_or]
%token <string Wrap.wrap> BOOL_AND "&&"  [@recover.expr Token.ghost_bool_and]
%token <string Wrap.wrap> QUOTE    "'"   [@recover.expr Token.ghost_quote]

(* Keywords *)

%token <string Wrap.wrap> Else   "else"   [@recover.expr Token.ghost_else]
%token <string Wrap.wrap> If     "if"     [@recover.expr Token.ghost_if]
%token <string Wrap.wrap> Let    "let"    [@recover.expr Token.ghost_let]
%token <string Wrap.wrap> Rec    "rec"    [@recover.expr Token.ghost_rec]
%token <string Wrap.wrap> Switch "switch" [@recover.expr Token.ghost_switch]
%token <string Wrap.wrap> Mod    "mod"    [@recover.expr Token.ghost_mod]
%token <string Wrap.wrap> Land   "land"   [@recover.expr Token.ghost_land]
%token <string Wrap.wrap> Lor    "lor"    [@recover.expr Token.ghost_lor]
%token <string Wrap.wrap> Lxor   "lxor"   [@recover.expr Token.ghost_lxor]
%token <string Wrap.wrap> Lsl    "lsl"    [@recover.expr Token.ghost_lsl]
%token <string Wrap.wrap> Lsr    "lsr"    [@recover.expr Token.ghost_lsr]
%token <string Wrap.wrap> Or     "or"     [@recover.expr Token.ghost_or]
%token <string Wrap.wrap> Type   "type"   [@recover.expr Token.ghost_type]
%token <string Wrap.wrap> Module "module" [@recover.expr Token.ghost_module]

(* Virtual tokens *)

%token <Region.t> ES6FUN [@recover.expr Region.ghost]

(* End of File *)

%token <string Wrap.wrap> EOF [@recover.expr Token.ghost_eof]

%%
