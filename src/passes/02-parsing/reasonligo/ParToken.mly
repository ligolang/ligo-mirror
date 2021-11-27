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
(* Tokens (mirroring those defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>" [@recover.expr Linemarker (Region.wrap_ghost (0, "<invalid-path>", None))]
%token                  <string Wrap.wrap> String    "<string>" [@recover.expr Wrap.ghost "<invalid-string-literal>"]
%token                  <string Wrap.wrap> Verbatim  "<verbatim>" [@recover.expr Wrap.ghost "<invalid-verbatim-literal>"]
%token        <(string * Hex.t) Wrap.wrap> Bytes     "<bytes>" [@recover.expr Wrap.ghost ("<invalid-bytes-literal>", `Hex "")]
%token          <(string * Z.t) Wrap.wrap> Int       "<int>" [@recover.expr Wrap.ghost ("<invalid-int-literal>", Z.zero)]
%token          <(string * Z.t) Wrap.wrap> Nat       "<nat>" [@recover.expr Wrap.ghost ("<invalid-nat-literal>", Z.zero)]
%token      <(string * Int64.t) Wrap.wrap> Mutez     "<mutez>" [@recover.expr Wrap.ghost ("<invalid-mutez-literal>", Int64.zero)]
%token                  <string Wrap.wrap> Ident     "<ident>" [@recover.expr Wrap.ghost "<invalid-ident>"]
%token                  <string Wrap.wrap> UIdent    "<uident>" [@recover.expr Wrap.ghost "<invalid-uident>"]
%token                 <Attr.t Region.reg> Attr      "[@attr]"     [@recover.expr Region.wrap_ghost ("<invalid-attr-literal>", None)]
%token      <string Region.reg Region.reg> Lang      "[%lang" [@recover.expr Region.wrap_ghost @@ Region.wrap_ghost "<invalid-lang-literal>"]

  (* Symbols *)

%token <string Wrap.wrap> MINUS   "-" [@recover.expr Wrap.ghost "-"]
%token <string Wrap.wrap> PLUS    "+" [@recover.expr Wrap.ghost "+"]
%token <string Wrap.wrap> SLASH   "/" [@recover.expr Wrap.ghost "/"]
%token <string Wrap.wrap> TIMES   "*" [@recover.expr Wrap.ghost "*"]

%token <string Wrap.wrap> LPAR     "(" [@recover.expr Wrap.ghost "("]
%token <string Wrap.wrap> RPAR     ")" [@recover.expr Wrap.ghost ")"]
%token <string Wrap.wrap> LBRACKET "[" [@recover.expr Wrap.ghost "["]
%token <string Wrap.wrap> RBRACKET "]" [@recover.expr Wrap.ghost "]"]
%token <string Wrap.wrap> LBRACE   "{" [@recover.expr Wrap.ghost "{"]
%token <string Wrap.wrap> RBRACE   "}" [@recover.expr Wrap.ghost "}"]

%token <string Wrap.wrap> PLUS2     "++" [@recover.expr Wrap.ghost "++"]
%token <string Wrap.wrap> DOT       "." [@recover.expr Wrap.ghost "."]
%token <string Wrap.wrap> ELLIPSIS  "..." [@recover.expr Wrap.ghost "..."]

%token <string Wrap.wrap> COMMA "," [@recover.expr Wrap.ghost ","]
%token <string Wrap.wrap> SEMI  ";" [@recover.expr Wrap.ghost ";"]
%token <string Wrap.wrap> COLON ":" [@recover.expr Wrap.ghost ":"]
%token <string Wrap.wrap> VBAR  "|" [@recover.expr Wrap.ghost "|"]

%token <string Wrap.wrap> WILD "_" [@recover.expr Wrap.ghost "_"]

%token <string Wrap.wrap> EQ    "=" [@recover.expr Wrap.ghost "="]
%token <string Wrap.wrap> EQ2   "==" [@recover.expr Wrap.ghost "=="]
%token <string Wrap.wrap> NE    "!=" [@recover.expr Wrap.ghost "!="]
%token <string Wrap.wrap> LT    "<" [@recover.expr Wrap.ghost "<"]
%token <string Wrap.wrap> GT    ">" [@recover.expr Wrap.ghost ">"]
%token <string Wrap.wrap> LE    "<=" [@recover.expr Wrap.ghost "<="]
%token <string Wrap.wrap> GE    ">=" [@recover.expr Wrap.ghost ">="]
%token <string Wrap.wrap> ARROW "=>" [@recover.expr Wrap.ghost "=>"]

%token <string Wrap.wrap> NOT      "!" [@recover.expr Wrap.ghost "!"]
%token <string Wrap.wrap> BOOL_OR  "||" [@recover.expr Wrap.ghost "||"]
%token <string Wrap.wrap> BOOL_AND "&&" [@recover.expr Wrap.ghost "&&"]
%token <string Wrap.wrap> QUOTE    "'" [@recover.expr Wrap.ghost "'"]

  (* Keywords *)

%token <string Wrap.wrap> Else   "else" [@recover.expr Wrap.ghost "else"]
%token <string Wrap.wrap> If     "if" [@recover.expr Wrap.ghost "if"]
%token <string Wrap.wrap> Let    "let" [@recover.expr Wrap.ghost "let"]
%token <string Wrap.wrap> Rec    "rec" [@recover.expr Wrap.ghost "rec"]
%token <string Wrap.wrap> Switch "switch" [@recover.expr Wrap.ghost "switch"]
%token <string Wrap.wrap> Mod    "mod" [@recover.expr Wrap.ghost "mod"]
%token <string Wrap.wrap> Land   "land" [@recover.expr Wrap.ghost "land"]
%token <string Wrap.wrap> Lor    "lor" [@recover.expr Wrap.ghost "lor"]
%token <string Wrap.wrap> Lxor   "lxor" [@recover.expr Wrap.ghost "lxor"]
%token <string Wrap.wrap> Lsl    "lsl" [@recover.expr Wrap.ghost "lsl"]
%token <string Wrap.wrap> Lsr    "lsr" [@recover.expr Wrap.ghost "lsr"]
%token <string Wrap.wrap> Or     "or" [@recover.expr Wrap.ghost "or"]
%token <string Wrap.wrap> Type   "type" [@recover.expr Wrap.ghost "type"]
%token <string Wrap.wrap> Module "module" [@recover.expr Wrap.ghost "module"]

  (* Virtual tokens *)

%token <string Wrap.wrap> EOF    [@recover.expr Wrap.ghost ""]
%token         <Region.t> ES6FUN [@recover.expr Region.ghost]

%%
