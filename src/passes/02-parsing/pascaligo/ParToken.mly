(* Tokens (mirroring these defined in module Token) *)

(* All external symbols here should be unqualified because this file
   is used by [menhir] that does not always insert the [%{..%}]
   header. So we work around it by the [-open Module] option in [dune]
   but symbols should be unqualified.

   Also, keep in mind that [ParToken.mly] and [Parser.mly] are merged
   into one file, and the header of [Parser.mly] affects this code.
   For example: [lexeme] type comes from [open CST] *)

%[@recover.prelude
  open Lexing_shared.Wrap
  module Region = Simple_utils.Region
 ]

(* Literals *)

%token           <LexerLib.Directive.t> Directive "<directive>" [@recover.expr Linemarker (Region.wrap_ghost (0, "<invalid-path>", None)) ]
%token                  <lexeme Wrap.t> String    "<string>"    [@recover.expr Wrap.ghost "<invalid-string-literal>"]
%token                  <lexeme Wrap.t> Verbatim  "<verbatim>"  [@recover.expr Wrap.ghost "<invalid-verbatim-literal>"]
%token        <(lexeme * Hex.t) Wrap.t> Bytes     "<bytes>"     [@recover.expr Wrap.ghost ("<invalid-bytes-literal>", `Hex "")]
%token          <(lexeme * Z.t) Wrap.t> Int       "<int>"       [@recover.expr Wrap.ghost ("<invalid-int-literal>", Z.zero)]
%token          <(lexeme * Z.t) Wrap.t> Nat       "<nat>"       [@recover.expr Wrap.ghost ("<invalid-nat-literal>", Z.zero)]
%token      <(lexeme * Int64.t) Wrap.t> Mutez     "<mutez>"     [@recover.expr Wrap.ghost ("<invalid-mutz-literal>", Z.zero)]
%token                  <lexeme Wrap.t> Ident     "<ident>"     [@recover.expr Wrap.ghost "<invalid-ident>"]
%token                  <lexeme Wrap.t> UIdent    "<uident>"    [@recover.expr Wrap.ghost "<invalid-uident>"]
%token      <Wrap.attribute Region.reg> Attr      "[@attr]"     [@recover.expr Wrap.ghost "<invalid-attr-literal>"]
%token      <lexeme Region.reg Region.reg> Lang   "[%lang"      [@recover.expr Region.wrap_ghost @@
                                                                 Region.wrap_ghost "<invalid-lang-literal>"]

  (* Symbols *)

%token <lexeme Wrap.t> SEMI     ";"   [@recover.expr Wrap.ghost ";"]
%token <lexeme Wrap.t> COMMA    ","   [@recover.expr Wrap.ghost ","]
%token <lexeme Wrap.t> LPAR     "("   [@recover.expr Wrap.ghost "("]
%token <lexeme Wrap.t> RPAR     ")"   [@recover.expr Wrap.ghost ")"]
%token <lexeme Wrap.t> LBRACE   "{"   [@recover.expr Wrap.ghost "{"]
%token <lexeme Wrap.t> RBRACE   "}"   [@recover.expr Wrap.ghost "}"]
%token <lexeme Wrap.t> LBRACKET "["   [@recover.expr Wrap.ghost "["]
%token <lexeme Wrap.t> RBRACKET "]"   [@recover.expr Wrap.ghost "]"]
%token <lexeme Wrap.t> SHARP    "#"   [@recover.expr Wrap.ghost "#"]
%token <lexeme Wrap.t> VBAR     "|"   [@recover.expr Wrap.ghost "|"]
%token <lexeme Wrap.t> ARROW    "->"  [@recover.expr Wrap.ghost "->"]
%token <lexeme Wrap.t> ASS      ":="  [@recover.expr Wrap.ghost ":="]
%token <lexeme Wrap.t> EQ       "="   [@recover.expr Wrap.ghost "="]
%token <lexeme Wrap.t> COLON    ":"   [@recover.expr Wrap.ghost ":"]
%token <lexeme Wrap.t> LT       "<"   [@recover.expr Wrap.ghost "<"]
%token <lexeme Wrap.t> LE       "<="  [@recover.expr Wrap.ghost "<="]
%token <lexeme Wrap.t> GT       ">"   [@recover.expr Wrap.ghost ">"]
%token <lexeme Wrap.t> GE       ">="  [@recover.expr Wrap.ghost ">="]
%token <lexeme Wrap.t> NE       "=/=" [@recover.expr Wrap.ghost "=/="]
%token <lexeme Wrap.t> PLUS     "+"   [@recover.expr Wrap.ghost "+"]
%token <lexeme Wrap.t> MINUS    "-"   [@recover.expr Wrap.ghost "-"]
%token <lexeme Wrap.t> SLASH    "/"   [@recover.expr Wrap.ghost "/"]
%token <lexeme Wrap.t> TIMES    "*"   [@recover.expr Wrap.ghost "*"]
%token <lexeme Wrap.t> DOT      "."   [@recover.expr Wrap.ghost "."]
%token <lexeme Wrap.t> WILD     "_"   [@recover.expr Wrap.ghost "_"]
%token <lexeme Wrap.t> CARET    "^"   [@recover.expr Wrap.ghost "^"]

  (* Keywords *)

%token <lexeme Wrap.t> And       "and"       [@recover.expr Wrap.ghost "and"]
%token <lexeme Wrap.t> Begin     "begin"     [@recover.expr Wrap.ghost "begin"]
%token <lexeme Wrap.t> BigMap    "big_map"   [@recover.expr Wrap.ghost "big_map"]
%token <lexeme Wrap.t> Block     "block"     [@recover.expr Wrap.ghost "block"]
%token <lexeme Wrap.t> Case      "case"      [@recover.expr Wrap.ghost "case"]
%token <lexeme Wrap.t> Const     "const"     [@recover.expr Wrap.ghost "const"]
%token <lexeme Wrap.t> Contains  "contains"  [@recover.expr Wrap.ghost "contains"]
%token <lexeme Wrap.t> Else      "else"      [@recover.expr Wrap.ghost "else"]
%token <lexeme Wrap.t> End       "end"       [@recover.expr Wrap.ghost "end"]
%token <lexeme Wrap.t> For       "for"       [@recover.expr Wrap.ghost "for"]
%token <lexeme Wrap.t> Function  "function"  [@recover.expr Wrap.ghost "function"]
%token <lexeme Wrap.t> Recursive "recursive" [@recover.expr Wrap.ghost "recursive"]
%token <lexeme Wrap.t> From      "from"      [@recover.expr Wrap.ghost "from"]
%token <lexeme Wrap.t> If        "if"        [@recover.expr Wrap.ghost "if"]
%token <lexeme Wrap.t> In        "in"        [@recover.expr Wrap.ghost "in"]
%token <lexeme Wrap.t> Is        "is"        [@recover.expr Wrap.ghost "is"]
%token <lexeme Wrap.t> List      "list"      [@recover.expr Wrap.ghost "list"]
%token <lexeme Wrap.t> Map       "map"       [@recover.expr Wrap.ghost "map"]
%token <lexeme Wrap.t> Mod       "mod"       [@recover.expr Wrap.ghost "mod"]
%token <lexeme Wrap.t> Nil       "nil"       [@recover.expr Wrap.ghost "nil"]
%token <lexeme Wrap.t> Not       "not"       [@recover.expr Wrap.ghost "not"]
%token <lexeme Wrap.t> Of        "of"        [@recover.expr Wrap.ghost "of"]
%token <lexeme Wrap.t> Or        "or"        [@recover.expr Wrap.ghost "or"]
%token <lexeme Wrap.t> Patch     "patch"     [@recover.expr Wrap.ghost "patch"]
%token <lexeme Wrap.t> Record    "record"    [@recover.expr Wrap.ghost "record"]
%token <lexeme Wrap.t> Remove    "remove"    [@recover.expr Wrap.ghost "remove"]
%token <lexeme Wrap.t> Set       "set"       [@recover.expr Wrap.ghost "set"]
%token <lexeme Wrap.t> Skip      "skip"      [@recover.expr Wrap.ghost "skip"]
%token <lexeme Wrap.t> Step      "step"      [@recover.expr Wrap.ghost "step"]
%token <lexeme Wrap.t> Then      "then"      [@recover.expr Wrap.ghost "then"]
%token <lexeme Wrap.t> To        "to"        [@recover.expr Wrap.ghost "to"]
%token <lexeme Wrap.t> Type      "type"      [@recover.expr Wrap.ghost "type"]
%token <lexeme Wrap.t> Var       "var"       [@recover.expr Wrap.ghost "var"]
%token <lexeme Wrap.t> While     "while"     [@recover.expr Wrap.ghost "while"]
%token <lexeme Wrap.t> With      "with"      [@recover.expr Wrap.ghost "with"]
%token <lexeme Wrap.t> Module    "module"    [@recover.expr Wrap.ghost "module"]

  (* Virtual tokens *)

%token <lexeme Wrap.t> EOF [@recover.expr Wrap.ghost ""]

%%
