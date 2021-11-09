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
%token                  <lexeme Wrap.t> String    "<string>"    [@recover.expr wrap_ghost "<invalid-string-literal>"]
%token                  <lexeme Wrap.t> Verbatim  "<verbatim>"  [@recover.expr wrap_ghost "<invalid-verbatim-literal>"]
%token        <(lexeme * Hex.t) Wrap.t> Bytes     "<bytes>"     [@recover.expr wrap_ghost ("<invalid-bytes-literal>", `Hex "")]
%token          <(lexeme * Z.t) Wrap.t> Int       "<int>"       [@recover.expr wrap_ghost ("<invalid-int-literal>", Z.zero)]
%token          <(lexeme * Z.t) Wrap.t> Nat       "<nat>"       [@recover.expr wrap_ghost ("<invalid-nat-literal>", Z.zero)]
%token          <(lexeme * Z.t) Wrap.t> Mutez     "<mutez>"     [@recover.expr wrap_ghost ("<invalid-mutz-literal>", Z.zero)]
%token                  <lexeme Wrap.t> Ident     "<ident>"     [@recover.expr wrap_ghost "<invalid-ident>"]
%token                  <lexeme Wrap.t> UIdent    "<uident>"    [@recover.expr wrap_ghost "<invalid-uident>"]
%token                  <string Wrap.t> Attr      "[@attr]"     [@recover.expr wrap_ghost "<invalid-attr-literal>"]
%token      <lexeme Region.reg Region.reg> Lang   "[%lang"      [@recover.expr wrap_ghost @@
                                                                 wrap_ghost "<invalid-lang-literal>"]

  (* Symbols *)

%token <lexeme Wrap.t> SEMI     ";"   [@recover.expr wrap_ghost ";"]
%token <lexeme Wrap.t> COMMA    ","   [@recover.expr wrap_ghost ","]
%token <lexeme Wrap.t> LPAR     "("   [@recover.expr wrap_ghost "("]
%token <lexeme Wrap.t> RPAR     ")"   [@recover.expr wrap_ghost ")"]
%token <lexeme Wrap.t> LBRACE   "{"   [@recover.expr wrap_ghost "{"]
%token <lexeme Wrap.t> RBRACE   "}"   [@recover.expr wrap_ghost "}"]
%token <lexeme Wrap.t> LBRACKET "["   [@recover.expr wrap_ghost "["]
%token <lexeme Wrap.t> RBRACKET "]"   [@recover.expr wrap_ghost "]"]
%token <lexeme Wrap.t> SHARP    "#"   [@recover.expr wrap_ghost "#"]
%token <lexeme Wrap.t> VBAR     "|"   [@recover.expr wrap_ghost "|"]
%token <lexeme Wrap.t> ARROW    "->"  [@recover.expr wrap_ghost "->"]
%token <lexeme Wrap.t> ASS      ":="  [@recover.expr wrap_ghost ":="]
%token <lexeme Wrap.t> EQ       "="   [@recover.expr wrap_ghost "="]
%token <lexeme Wrap.t> COLON    ":"   [@recover.expr wrap_ghost ":"]
%token <lexeme Wrap.t> LT       "<"   [@recover.expr wrap_ghost "<"]
%token <lexeme Wrap.t> LE       "<="  [@recover.expr wrap_ghost "<="]
%token <lexeme Wrap.t> GT       ">"   [@recover.expr wrap_ghost ">"]
%token <lexeme Wrap.t> GE       ">="  [@recover.expr wrap_ghost ">="]
%token <lexeme Wrap.t> NE       "=/=" [@recover.expr wrap_ghost "=/="]
%token <lexeme Wrap.t> PLUS     "+"   [@recover.expr wrap_ghost "+"]
%token <lexeme Wrap.t> MINUS    "-"   [@recover.expr wrap_ghost "-"]
%token <lexeme Wrap.t> SLASH    "/"   [@recover.expr wrap_ghost "/"]
%token <lexeme Wrap.t> TIMES    "*"   [@recover.expr wrap_ghost "*"]
%token <lexeme Wrap.t> DOT      "."   [@recover.expr wrap_ghost "."]
%token <lexeme Wrap.t> WILD     "_"   [@recover.expr wrap_ghost "_"]
%token <lexeme Wrap.t> CARET    "^"   [@recover.expr wrap_ghost "^"]

  (* Keywords *)

%token <lexeme Wrap.t> And       "and"       [@recover.expr wrap_ghost "and"]
%token <lexeme Wrap.t> Begin     "begin"     [@recover.expr wrap_ghost "begin"]
%token <lexeme Wrap.t> BigMap    "big_map"   [@recover.expr wrap_ghost "big_map"]
%token <lexeme Wrap.t> Block     "block"     [@recover.expr wrap_ghost "block"]
%token <lexeme Wrap.t> Case      "case"      [@recover.expr wrap_ghost "case"]
%token <lexeme Wrap.t> Const     "const"     [@recover.expr wrap_ghost "const"]
%token <lexeme Wrap.t> Contains  "contains"  [@recover.expr wrap_ghost "contains"]
%token <lexeme Wrap.t> Else      "else"      [@recover.expr wrap_ghost "else"]
%token <lexeme Wrap.t> End       "end"       [@recover.expr wrap_ghost "end"]
%token <lexeme Wrap.t> For       "for"       [@recover.expr wrap_ghost "for"]
%token <lexeme Wrap.t> Function  "function"  [@recover.expr wrap_ghost "function"]
%token <lexeme Wrap.t> Recursive "recursive" [@recover.expr wrap_ghost "recursive"]
%token <lexeme Wrap.t> From      "from"      [@recover.expr wrap_ghost "from"]
%token <lexeme Wrap.t> If        "if"        [@recover.expr wrap_ghost "if"]
%token <lexeme Wrap.t> In        "in"        [@recover.expr wrap_ghost "in"]
%token <lexeme Wrap.t> Is        "is"        [@recover.expr wrap_ghost "is"]
%token <lexeme Wrap.t> List      "list"      [@recover.expr wrap_ghost "list"]
%token <lexeme Wrap.t> Map       "map"       [@recover.expr wrap_ghost "map"]
%token <lexeme Wrap.t> Mod       "mod"       [@recover.expr wrap_ghost "mod"]
%token <lexeme Wrap.t> Nil       "nil"       [@recover.expr wrap_ghost "nil"]
%token <lexeme Wrap.t> Not       "not"       [@recover.expr wrap_ghost "not"]
%token <lexeme Wrap.t> Of        "of"        [@recover.expr wrap_ghost "of"]
%token <lexeme Wrap.t> Or        "or"        [@recover.expr wrap_ghost "or"]
%token <lexeme Wrap.t> Patch     "patch"     [@recover.expr wrap_ghost "patch"]
%token <lexeme Wrap.t> Record    "record"    [@recover.expr wrap_ghost "record"]
%token <lexeme Wrap.t> Remove    "remove"    [@recover.expr wrap_ghost "remove"]
%token <lexeme Wrap.t> Set       "set"       [@recover.expr wrap_ghost "set"]
%token <lexeme Wrap.t> Skip      "skip"      [@recover.expr wrap_ghost "skip"]
%token <lexeme Wrap.t> Step      "step"      [@recover.expr wrap_ghost "step"]
%token <lexeme Wrap.t> Then      "then"      [@recover.expr wrap_ghost "then"]
%token <lexeme Wrap.t> To        "to"        [@recover.expr wrap_ghost "to"]
%token <lexeme Wrap.t> Type      "type"      [@recover.expr wrap_ghost "type"]
%token <lexeme Wrap.t> Var       "var"       [@recover.expr wrap_ghost "var"]
%token <lexeme Wrap.t> While     "while"     [@recover.expr wrap_ghost "while"]
%token <lexeme Wrap.t> With      "with"      [@recover.expr wrap_ghost "with"]
%token <lexeme Wrap.t> Module    "module"    [@recover.expr wrap_ghost "module"]

  (* Virtual tokens *)

%token <lexeme Wrap.t> EOF [@recover.expr wrap_ghost ""]

%%
