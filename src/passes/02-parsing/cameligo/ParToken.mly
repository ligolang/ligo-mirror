(* Tokens (mirroring these defined in module Token) *)

(* All external symbols here should be unqualified because this file
   is used by [menhir] that does not always insert the [%{..%}]
   header. So we work around it by the [-open Module] option in [dune]
   but symbols should be unqualified.

   Also, keep in mind that [ParToken.mly] and [Parser.mly] are merged
   into one file, and the header of [Parser.mly] affects this code. *)

%[@recover.prelude
  open Lexing_shared.Wrap
  module Region = Simple_utils.Region
 ]
(* Tokens (mirroring thise defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>" [@recover.expr Linemarker (Region.wrap_ghost (0, "<invalid-path>", None)) ]
%token                  <string Wrap.wrap> String    "<string>" [@recover.expr wrap_ghost "<invalid-string-literal>"]
%token                  <string Wrap.wrap> Verbatim  "<verbatim>" [@recover.expr wrap_ghost "<invalid-verbatim-literal>"]
%token        <(string * Hex.t) Wrap.wrap> Bytes     "<bytes>" [@recover.expr wrap_ghost ("<invalid-bytes-literal>", `Hex "")]
%token          <(string * Z.t) Wrap.wrap> Int       "<int>" [@recover.expr wrap_ghost ("<invalid-int-literal>", Z.zero)]
%token          <(string * Z.t) Wrap.wrap> Nat       "<nat>" [@recover.expr wrap_ghost ("<invalid-nat-literal>", Z.zero)]
%token      <(string * Int64.t) Wrap.wrap> Mutez     "<mutez>" [@recover.expr wrap_ghost ("<invalid-mutez-literal>", Z.zero)]
%token                  <string Wrap.wrap> Ident     "<ident>" [@recover.expr wrap_ghost "<invalid-ident>"]
%token                  <string Wrap.wrap> UIdent    "<uident>" [@recover.expr wrap_ghost "<invalid-uident>"]
%token                 <Attr.t Region.reg> Attr      "[@attr]" [@recover.expr wrap_ghost "<invalid-attr-literal>"]
%token      <string Region.reg Region.reg> Lang      "[%lang" [@recover.expr Region.wrap_ghost @@ Region.wrap_ghost "<invalid-lang-literal>"]

  (* Symbols *)

%token <string Wrap.wrap> MINUS   "-" [@recover.expr wrap_ghost "-"]
%token <string Wrap.wrap> PLUS    "+" [@recover.expr wrap_ghost "+"]
%token <string Wrap.wrap> SLASH   "/" [@recover.expr wrap_ghost "/"]
%token <string Wrap.wrap> TIMES   "*" [@recover.expr wrap_ghost "*"]

%token <string Wrap.wrap> LPAR     "(" [@recover.expr wrap_ghost "("]
%token <string Wrap.wrap> RPAR     ")" [@recover.expr wrap_ghost ")"]
%token <string Wrap.wrap> LBRACKET "[" [@recover.expr wrap_ghost "["]
%token <string Wrap.wrap> RBRACKET "]" [@recover.expr wrap_ghost "]"]
%token <string Wrap.wrap> LBRACE   "{" [@recover.expr wrap_ghost "{"]
%token <string Wrap.wrap> RBRACE   "}" [@recover.expr wrap_ghost "}"]

%token <string Wrap.wrap> ARROW "->" [@recover.expr wrap_ghost "->"]
%token <string Wrap.wrap> CONS  "::" [@recover.expr wrap_ghost "::"]
%token <string Wrap.wrap> CARET "^" [@recover.expr wrap_ghost "^"]
(*%token <string Wrap.wrap> APPEND "@" *)
%token <string Wrap.wrap> DOT   "." [@recover.expr wrap_ghost "."]

%token <string Wrap.wrap> COMMA "," [@recover.expr wrap_ghost ","]
%token <string Wrap.wrap> SEMI  ";" [@recover.expr wrap_ghost ";"]
%token <string Wrap.wrap> COLON ":" [@recover.expr wrap_ghost ":"]
%token <string Wrap.wrap> VBAR  "|" [@recover.expr wrap_ghost "|"]

%token <string Wrap.wrap> WILD  "_" [@recover.expr wrap_ghost "_"]

%token <string Wrap.wrap> EQ "=" [@recover.expr wrap_ghost "="]
%token <string Wrap.wrap> NE "<>" [@recover.expr wrap_ghost "<>"]
%token <string Wrap.wrap> LT "<" [@recover.expr wrap_ghost "<"]
%token <string Wrap.wrap> GT ">" [@recover.expr wrap_ghost ">"]
%token <string Wrap.wrap> LE "<=" [@recover.expr wrap_ghost "<="]
%token <string Wrap.wrap> GE ">=" [@recover.expr wrap_ghost ">="]

%token <string Wrap.wrap> BOOL_OR  "||" [@recover.expr wrap_ghost "||"]
%token <string Wrap.wrap> BOOL_AND "&&" [@recover.expr wrap_ghost "&&"]
%token <string Wrap.wrap> QUOTE    "'" [@recover.expr wrap_ghost "'"]

 (* Keywords *)

(*%token And*)
%token <string Wrap.wrap> Begin  "begin" [@recover.expr wrap_ghost "begin"]
%token <string Wrap.wrap> Else   "else" [@recover.expr wrap_ghost "else"]
%token <string Wrap.wrap> End    "end" [@recover.expr wrap_ghost "end"]
%token <string Wrap.wrap> Fun    "fun" [@recover.expr wrap_ghost "fun"]
%token <string Wrap.wrap> Rec    "rec" [@recover.expr wrap_ghost "rec"]
%token <string Wrap.wrap> If     "if" [@recover.expr wrap_ghost "if"]
%token <string Wrap.wrap> In     "in" [@recover.expr wrap_ghost "in"]
%token <string Wrap.wrap> Let    "let" [@recover.expr wrap_ghost "let"]
%token <string Wrap.wrap> Match  "match" [@recover.expr wrap_ghost "match"]
%token <string Wrap.wrap> Mod    "mod" [@recover.expr wrap_ghost "mod"]
%token <string Wrap.wrap> Land   "land" [@recover.expr wrap_ghost "land"]
%token <string Wrap.wrap> Lor    "lor" [@recover.expr wrap_ghost "lor"]
%token <string Wrap.wrap> Lxor   "lxor" [@recover.expr wrap_ghost "lxor"]
%token <string Wrap.wrap> Lsl    "lsl" [@recover.expr wrap_ghost "lsl"]
%token <string Wrap.wrap> Lsr    "lsr" [@recover.expr wrap_ghost "lsr"]
%token <string Wrap.wrap> Not    "not" [@recover.expr wrap_ghost "not"]
%token <string Wrap.wrap> Of     "of" [@recover.expr wrap_ghost "of"]
%token <string Wrap.wrap> Or     "or" [@recover.expr wrap_ghost "or"]
%token <string Wrap.wrap> Then   "then" [@recover.expr wrap_ghost "then"]
%token <string Wrap.wrap> Type   "type" [@recover.expr wrap_ghost "type"]
%token <string Wrap.wrap> With   "with" [@recover.expr wrap_ghost "with"]
%token <string Wrap.wrap> Module "module" [@recover.expr wrap_ghost "module"]
%token <string Wrap.wrap> Struct "struct" [@recover.expr wrap_ghost "struct"]

  (* Virtual tokens *)

%token <string Wrap.wrap> EOF [@recover.expr wrap_ghost ""]

%%
