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
(* Tokens (mirroring thise defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>" [@recover.expr Linemarker (Region.wrap_ghost (0, "<invalid-path>", None)) ]
%token                  <string Wrap.t> String    "<string>" [@recover.expr Wrap.ghost "<invalid-string-literal>"]
%token                  <string Wrap.t> Verbatim  "<verbatim>" [@recover.expr Wrap.ghost "<invalid-verbatim-literal>"]
%token        <(string * Hex.t) Wrap.t> Bytes     "<bytes>" [@recover.expr Wrap.ghost ("<invalid-bytes-literal>", `Hex "")]
%token          <(string * Z.t) Wrap.t> Int       "<int>" [@recover.expr Wrap.ghost ("<invalid-int-literal>", Z.zero)]
%token          <(string * Z.t) Wrap.t> Nat       "<nat>" [@recover.expr Wrap.ghost ("<invalid-nat-literal>", Z.zero)]
%token      <(string * Int64.t) Wrap.t> Mutez     "<mutez>" [@recover.expr Wrap.ghost ("<invalid-mutez-literal>", Int64.zero)]
%token                  <string Wrap.t> Ident     "<ident>" [@recover.expr Wrap.ghost "<invalid-ident>"]
%token                  <string Wrap.t> UIdent    "<uident>" [@recover.expr Wrap.ghost "<invalid-uident>"]
%token              <Attr.t Region.reg> Attr      "[@attr]"     [@recover.expr Region.wrap_ghost ("<invalid-attr-literal>", None)]
%token      <string Region.reg Region.reg> Lang      "[%lang" [@recover.expr Region.wrap_ghost @@ Region.wrap_ghost "<invalid-lang-literal>"]

  (* Symbols *)

%token <string Wrap.t> MINUS   "-" [@recover.expr Wrap.ghost "-"]
%token <string Wrap.t> PLUS    "+" [@recover.expr Wrap.ghost "+"]
%token <string Wrap.t> SLASH   "/" [@recover.expr Wrap.ghost "/"]
%token <string Wrap.t> TIMES   "*" [@recover.expr Wrap.ghost "*"]

%token <string Wrap.t> LPAR     "(" [@recover.expr Wrap.ghost "("]
%token <string Wrap.t> RPAR     ")" [@recover.expr Wrap.ghost ")"]
%token <string Wrap.t> LBRACKET "[" [@recover.expr Wrap.ghost "["]
%token <string Wrap.t> RBRACKET "]" [@recover.expr Wrap.ghost "]"]
%token <string Wrap.t> LBRACE   "{" [@recover.expr Wrap.ghost "{"]
%token <string Wrap.t> RBRACE   "}" [@recover.expr Wrap.ghost "}"]

%token <string Wrap.t> ARROW "->" [@recover.expr Wrap.ghost "->"]
%token <string Wrap.t> CONS  "::" [@recover.expr Wrap.ghost "::"]
%token <string Wrap.t> CARET "^" [@recover.expr Wrap.ghost "^"]
(*%token <string Wrap.t> APPEND "@" *)
%token <string Wrap.t> DOT   "." [@recover.expr Wrap.ghost "."]

%token <string Wrap.t> COMMA "," [@recover.expr Wrap.ghost ","]
%token <string Wrap.t> SEMI  ";" [@recover.expr Wrap.ghost ";"]
%token <string Wrap.t> COLON ":" [@recover.expr Wrap.ghost ":"]
%token <string Wrap.t> VBAR  "|" [@recover.expr Wrap.ghost "|"]

%token <string Wrap.t> WILD  "_" [@recover.expr Wrap.ghost "_"]

%token <string Wrap.t> EQ "=" [@recover.expr Wrap.ghost "="]
%token <string Wrap.t> NE "<>" [@recover.expr Wrap.ghost "<>"]
%token <string Wrap.t> LT "<" [@recover.expr Wrap.ghost "<"]
%token <string Wrap.t> GT ">" [@recover.expr Wrap.ghost ">"]
%token <string Wrap.t> LE "<=" [@recover.expr Wrap.ghost "<="]
%token <string Wrap.t> GE ">=" [@recover.expr Wrap.ghost ">="]

%token <string Wrap.t> BOOL_OR  "||" [@recover.expr Wrap.ghost "||"]
%token <string Wrap.t> BOOL_AND "&&" [@recover.expr Wrap.ghost "&&"]
%token <string Wrap.t> QUOTE    "'" [@recover.expr Wrap.ghost "'"]

 (* Keywords *)

(*%token And*)
%token <string Wrap.t> Begin  "begin" [@recover.expr Wrap.ghost "begin"]
%token <string Wrap.t> Else   "else" [@recover.expr Wrap.ghost "else"]
%token <string Wrap.t> End    "end" [@recover.expr Wrap.ghost "end"]
%token <string Wrap.t> Fun    "fun" [@recover.expr Wrap.ghost "fun"]
%token <string Wrap.t> Rec    "rec" [@recover.expr Wrap.ghost "rec"]
%token <string Wrap.t> If     "if" [@recover.expr Wrap.ghost "if"]
%token <string Wrap.t> In     "in" [@recover.expr Wrap.ghost "in"]
%token <string Wrap.t> Let    "let" [@recover.expr Wrap.ghost "let"]
%token <string Wrap.t> Match  "match" [@recover.expr Wrap.ghost "match"]
%token <string Wrap.t> Mod    "mod" [@recover.expr Wrap.ghost "mod"]
%token <string Wrap.t> Land   "land" [@recover.expr Wrap.ghost "land"]
%token <string Wrap.t> Lor    "lor" [@recover.expr Wrap.ghost "lor"]
%token <string Wrap.t> Lxor   "lxor" [@recover.expr Wrap.ghost "lxor"]
%token <string Wrap.t> Lsl    "lsl" [@recover.expr Wrap.ghost "lsl"]
%token <string Wrap.t> Lsr    "lsr" [@recover.expr Wrap.ghost "lsr"]
%token <string Wrap.t> Not    "not" [@recover.expr Wrap.ghost "not"]
%token <string Wrap.t> Of     "of" [@recover.expr Wrap.ghost "of"]
%token <string Wrap.t> Or     "or" [@recover.expr Wrap.ghost "or"]
%token <string Wrap.t> Then   "then" [@recover.expr Wrap.ghost "then"]
%token <string Wrap.t> Type   "type" [@recover.expr Wrap.ghost "type"]
%token <string Wrap.t> With   "with" [@recover.expr Wrap.ghost "with"]
%token <string Wrap.t> Module "module" [@recover.expr Wrap.ghost "module"]
%token <string Wrap.t> Struct "struct" [@recover.expr Wrap.ghost "struct"]

  (* Virtual tokens *)

%token <string Wrap.t> EOF [@recover.expr Wrap.ghost ""]

%%
