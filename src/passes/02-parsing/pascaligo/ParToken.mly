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
  module Token     = Lexing_pascaligo.Token

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

%token         <LexerLib.Directive.t> Directive "<directive>" [@recover.expr ghost_Linemarker]
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

%token <string Wrap.t> SEMI     ";"   [@recover.expr Token.ghost_semi]
%token <string Wrap.t> COMMA    ","   [@recover.expr Token.ghost_comma]
%token <string Wrap.t> LPAR     "("   [@recover.expr Token.ghost_lpar]
%token <string Wrap.t> RPAR     ")"   [@recover.expr Token.ghost_rpar]
%token <string Wrap.t> LBRACE   "{"   [@recover.expr Token.ghost_lbrace]
%token <string Wrap.t> RBRACE   "}"   [@recover.expr Token.ghost_rbrace]
%token <string Wrap.t> LBRACKET "["   [@recover.expr Token.ghost_lbracket]
%token <string Wrap.t> RBRACKET "]"   [@recover.expr Token.ghost_rbracket]
%token <string Wrap.t> SHARP    "#"   [@recover.expr Token.ghost_sharp]
%token <string Wrap.t> VBAR     "|"   [@recover.expr Token.ghost_vbar]
%token <string Wrap.t> ARROW    "->"  [@recover.expr Token.ghost_arrow]
%token <string Wrap.t> ASS      ":="  [@recover.expr Token.ghost_ass]
%token <string Wrap.t> EQ       "="   [@recover.expr Token.ghost_eq]
%token <string Wrap.t> COLON    ":"   [@recover.expr Token.ghost_colon]
%token <string Wrap.t> LT       "<"   [@recover.expr Token.ghost_lt]
%token <string Wrap.t> LE       "<="  [@recover.expr Token.ghost_le]
%token <string Wrap.t> GT       ">"   [@recover.expr Token.ghost_gt]
%token <string Wrap.t> GE       ">="  [@recover.expr Token.ghost_ge]
%token <string Wrap.t> NE       "=/=" [@recover.expr Token.ghost_ne]
%token <string Wrap.t> PLUS     "+"   [@recover.expr Token.ghost_plus]
%token <string Wrap.t> MINUS    "-"   [@recover.expr Token.ghost_minus]
%token <string Wrap.t> SLASH    "/"   [@recover.expr Token.ghost_slash]
%token <string Wrap.t> TIMES    "*"   [@recover.expr Token.ghost_times]
%token <string Wrap.t> DOT      "."   [@recover.expr Token.ghost_dot]
%token <string Wrap.t> WILD     "_"   [@recover.expr Token.ghost_wild]
%token <string Wrap.t> CARET    "^"   [@recover.expr Token.ghost_caret]

(* Keywords *)

%token <string Wrap.t> And       "and"       [@recover.expr Token.ghost_and]
%token <string Wrap.t> Begin     "begin"     [@recover.expr Token.ghost_begin]
%token <string Wrap.t> BigMap    "big_map"   [@recover.expr Token.ghost_big_map]
%token <string Wrap.t> Block     "block"     [@recover.expr Token.ghost_block]
%token <string Wrap.t> Case      "case"      [@recover.expr Token.ghost_case]
%token <string Wrap.t> Const     "const"     [@recover.expr Token.ghost_const]
%token <string Wrap.t> Contains  "contains"  [@recover.expr Token.ghost_contains]
%token <string Wrap.t> Else      "else"      [@recover.expr Token.ghost_else]
%token <string Wrap.t> End       "end"       [@recover.expr Token.ghost_end]
%token <string Wrap.t> For       "for"       [@recover.expr Token.ghost_for]
%token <string Wrap.t> Function  "function"  [@recover.expr Token.ghost_function]
%token <string Wrap.t> Recursive "recursive" [@recover.expr Token.ghost_recursive]
%token <string Wrap.t> From      "from"      [@recover.expr Token.ghost_from]
%token <string Wrap.t> If        "if"        [@recover.expr Token.ghost_if]
%token <string Wrap.t> In        "in"        [@recover.expr Token.ghost_in]
%token <string Wrap.t> Is        "is"        [@recover.expr Token.ghost_is]
%token <string Wrap.t> List      "list"      [@recover.expr Token.ghost_list]
%token <string Wrap.t> Map       "map"       [@recover.expr Token.ghost_map]
%token <string Wrap.t> Mod       "mod"       [@recover.expr Token.ghost_mod]
%token <string Wrap.t> Nil       "nil"       [@recover.expr Token.ghost_nil]
%token <string Wrap.t> Not       "not"       [@recover.expr Token.ghost_not]
%token <string Wrap.t> Of        "of"        [@recover.expr Token.ghost_of]
%token <string Wrap.t> Or        "or"        [@recover.expr Token.ghost_or]
%token <string Wrap.t> Patch     "patch"     [@recover.expr Token.ghost_patch]
%token <string Wrap.t> Record    "record"    [@recover.expr Token.ghost_record]
%token <string Wrap.t> Remove    "remove"    [@recover.expr Token.ghost_remove]
%token <string Wrap.t> Set       "set"       [@recover.expr Token.ghost_set]
%token <string Wrap.t> Skip      "skip"      [@recover.expr Token.ghost_skip]
%token <string Wrap.t> Step      "step"      [@recover.expr Token.ghost_step]
%token <string Wrap.t> Then      "then"      [@recover.expr Token.ghost_then]
%token <string Wrap.t> To        "to"        [@recover.expr Token.ghost_to]
%token <string Wrap.t> Type      "type"      [@recover.expr Token.ghost_type]
%token <string Wrap.t> Var       "var"       [@recover.expr Token.ghost_var]
%token <string Wrap.t> While     "while"     [@recover.expr Token.ghost_while]
%token <string Wrap.t> With      "with"      [@recover.expr Token.ghost_with]
%token <string Wrap.t> Module    "module"    [@recover.expr Token.ghost_module]

(* Virtual tokens *)

%token <string Wrap.t> EOF [@recover.expr Token.ghost_eof]

%%
