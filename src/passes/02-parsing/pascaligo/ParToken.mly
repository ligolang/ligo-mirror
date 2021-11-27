(* Tokens (mirroring these defined in module Token) *)

(* All external symbols here should be unqualified because this file
   is used by [menhir] that does not always insert the [%{..%}]
   header. So we work around it by the [-open Module] option in [dune]
   but symbols should be unqualified.

   Also, keep in mind that [ParToken.mly] and [Parser.mly] are merged
   into one file, and the header of [Parser.mly] affects this code. *)

%[@recover.prelude
  (* See [dune] file for [-open] flags *)
  module Region = Simple_utils.Region
 ]

(* Literals *)

%token           <LexerLib.Directive.t> Directive "<directive>" [@recover.expr Linemarker (Region.wrap_ghost (0, "<invalid-path>", None)) ]
%token                  <string Wrap.t> String    "<string>"    [@recover.expr Wrap.ghost "<invalid-string-literal>"]
%token                  <string Wrap.t> Verbatim  "<verbatim>"  [@recover.expr Wrap.ghost "<invalid-verbatim-literal>"]
%token        <(string * Hex.t) Wrap.t> Bytes     "<bytes>"     [@recover.expr Wrap.ghost ("<invalid-bytes-literal>", `Hex "")]
%token          <(string * Z.t) Wrap.t> Int       "<int>"       [@recover.expr Wrap.ghost ("<invalid-int-literal>", Z.zero)]
%token          <(string * Z.t) Wrap.t> Nat       "<nat>"       [@recover.expr Wrap.ghost ("<invalid-nat-literal>", Z.zero)]
%token      <(string * Int64.t) Wrap.t> Mutez     "<mutez>"     [@recover.expr Wrap.ghost ("<invalid-mutz-literal>", Int64.zero)]
%token                  <string Wrap.t> Ident     "<ident>"     [@recover.expr Wrap.ghost "<invalid-ident>"]
%token                  <string Wrap.t> UIdent    "<uident>"    [@recover.expr Wrap.ghost "<invalid-uident>"]
%token              <Attr.t Region.reg> Attr      "[@attr]"     [@recover.expr Region.wrap_ghost ("<invalid-attr-literal>", None)]
%token   <string Region.reg Region.reg> Lang      "[%lang"      [@recover.expr Region.wrap_ghost @@ Region.wrap_ghost "<invalid-lang-literal>"]

  (* Symbols *)

%token <string Wrap.t> SEMI     ";"   [@recover.expr Wrap.ghost ";"]
%token <string Wrap.t> COMMA    ","   [@recover.expr Wrap.ghost ","]
%token <string Wrap.t> LPAR     "("   [@recover.expr Wrap.ghost "("]
%token <string Wrap.t> RPAR     ")"   [@recover.expr Wrap.ghost ")"]
%token <string Wrap.t> LBRACE   "{"   [@recover.expr Wrap.ghost "{"]
%token <string Wrap.t> RBRACE   "}"   [@recover.expr Wrap.ghost "}"]
%token <string Wrap.t> LBRACKET "["   [@recover.expr Wrap.ghost "["]
%token <string Wrap.t> RBRACKET "]"   [@recover.expr Wrap.ghost "]"]
%token <string Wrap.t> SHARP    "#"   [@recover.expr Wrap.ghost "#"]
%token <string Wrap.t> VBAR     "|"   [@recover.expr Wrap.ghost "|"]
%token <string Wrap.t> ARROW    "->"  [@recover.expr Wrap.ghost "->"]
%token <string Wrap.t> ASS      ":="  [@recover.expr Wrap.ghost ":="]
%token <string Wrap.t> EQ       "="   [@recover.expr Wrap.ghost "="]
%token <string Wrap.t> COLON    ":"   [@recover.expr Wrap.ghost ":"]
%token <string Wrap.t> LT       "<"   [@recover.expr Wrap.ghost "<"]
%token <string Wrap.t> LE       "<="  [@recover.expr Wrap.ghost "<="]
%token <string Wrap.t> GT       ">"   [@recover.expr Wrap.ghost ">"]
%token <string Wrap.t> GE       ">="  [@recover.expr Wrap.ghost ">="]
%token <string Wrap.t> NE       "=/=" [@recover.expr Wrap.ghost "=/="]
%token <string Wrap.t> PLUS     "+"   [@recover.expr Wrap.ghost "+"]
%token <string Wrap.t> MINUS    "-"   [@recover.expr Wrap.ghost "-"]
%token <string Wrap.t> SLASH    "/"   [@recover.expr Wrap.ghost "/"]
%token <string Wrap.t> TIMES    "*"   [@recover.expr Wrap.ghost "*"]
%token <string Wrap.t> DOT      "."   [@recover.expr Wrap.ghost "."]
%token <string Wrap.t> WILD     "_"   [@recover.expr Wrap.ghost "_"]
%token <string Wrap.t> CARET    "^"   [@recover.expr Wrap.ghost "^"]

  (* Keywords *)

%token <string Wrap.t> And       "and"       [@recover.expr Wrap.ghost "and"]
%token <string Wrap.t> Begin     "begin"     [@recover.expr Wrap.ghost "begin"]
%token <string Wrap.t> BigMap    "big_map"   [@recover.expr Wrap.ghost "big_map"]
%token <string Wrap.t> Block     "block"     [@recover.expr Wrap.ghost "block"]
%token <string Wrap.t> Case      "case"      [@recover.expr Wrap.ghost "case"]
%token <string Wrap.t> Const     "const"     [@recover.expr Wrap.ghost "const"]
%token <string Wrap.t> Contains  "contains"  [@recover.expr Wrap.ghost "contains"]
%token <string Wrap.t> Else      "else"      [@recover.expr Wrap.ghost "else"]
%token <string Wrap.t> End       "end"       [@recover.expr Wrap.ghost "end"]
%token <string Wrap.t> For       "for"       [@recover.expr Wrap.ghost "for"]
%token <string Wrap.t> Function  "function"  [@recover.expr Wrap.ghost "function"]
%token <string Wrap.t> Recursive "recursive" [@recover.expr Wrap.ghost "recursive"]
%token <string Wrap.t> From      "from"      [@recover.expr Wrap.ghost "from"]
%token <string Wrap.t> If        "if"        [@recover.expr Wrap.ghost "if"]
%token <string Wrap.t> In        "in"        [@recover.expr Wrap.ghost "in"]
%token <string Wrap.t> Is        "is"        [@recover.expr Wrap.ghost "is"]
%token <string Wrap.t> List      "list"      [@recover.expr Wrap.ghost "list"]
%token <string Wrap.t> Map       "map"       [@recover.expr Wrap.ghost "map"]
%token <string Wrap.t> Mod       "mod"       [@recover.expr Wrap.ghost "mod"]
%token <string Wrap.t> Nil       "nil"       [@recover.expr Wrap.ghost "nil"]
%token <string Wrap.t> Not       "not"       [@recover.expr Wrap.ghost "not"]
%token <string Wrap.t> Of        "of"        [@recover.expr Wrap.ghost "of"]
%token <string Wrap.t> Or        "or"        [@recover.expr Wrap.ghost "or"]
%token <string Wrap.t> Patch     "patch"     [@recover.expr Wrap.ghost "patch"]
%token <string Wrap.t> Record    "record"    [@recover.expr Wrap.ghost "record"]
%token <string Wrap.t> Remove    "remove"    [@recover.expr Wrap.ghost "remove"]
%token <string Wrap.t> Set       "set"       [@recover.expr Wrap.ghost "set"]
%token <string Wrap.t> Skip      "skip"      [@recover.expr Wrap.ghost "skip"]
%token <string Wrap.t> Step      "step"      [@recover.expr Wrap.ghost "step"]
%token <string Wrap.t> Then      "then"      [@recover.expr Wrap.ghost "then"]
%token <string Wrap.t> To        "to"        [@recover.expr Wrap.ghost "to"]
%token <string Wrap.t> Type      "type"      [@recover.expr Wrap.ghost "type"]
%token <string Wrap.t> Var       "var"       [@recover.expr Wrap.ghost "var"]
%token <string Wrap.t> While     "while"     [@recover.expr Wrap.ghost "while"]
%token <string Wrap.t> With      "with"      [@recover.expr Wrap.ghost "with"]
%token <string Wrap.t> Module    "module"    [@recover.expr Wrap.ghost "module"]

  (* Virtual tokens *)

%token <string Wrap.t> EOF [@recover.expr Wrap.ghost ""]

%%
