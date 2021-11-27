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

%token               <LexerLib.Directive.t> Directive "<directive>" [@recover.expr Linemarker (Region.wrap_ghost (0, "<invalid-path>", None)) ]
%token                  <string Wrap.wrap> BlockCom "<block_comment>" [@recover.expr Wrap.ghost "<invalid-block-comment>"]
%token                  <string Wrap.wrap> LineCom  "<line_comment>" [@recover.expr Wrap.ghost "<invalid-line-comment>"]
%token                  <string Wrap.wrap> String   "<string>" [@recover.expr Wrap.ghost "<invalid-string-literal>"]
%token                  <string Wrap.wrap> Verbatim "<verbatim>" [@recover.expr Wrap.ghost "<invalid-verbatim-literal>"]
%token        <(string * Hex.t) Wrap.wrap> Bytes    "<bytes>" [@recover.expr Wrap.ghost ("<invalid-bytes-literal>", `Hex "")]
%token          <(string * Z.t) Wrap.wrap> Int      "<int>" [@recover.expr Wrap.ghost ("<invalid-int-literal>", Z.zero)]
(*
%token          <(string * Z.t) Wrap.wrap> Nat      "<nat>"
%token      <(string * Int64.t) Wrap.wrap> Mutez    "<mutez>"
*)
%token                  <string Wrap.wrap> Ident    "<ident>" [@recover.expr Wrap.ghost "<invalid-ident>"]
%token                  <string Wrap.wrap> UIdent   "<uident>" [@recover.expr Wrap.ghost "<invalid-uident>"]
%token                 <Attr.t Region.reg> Attr     "[@attr]"     [@recover.expr Region.wrap_ghost ("<invalid-attr-literal>", None)]
(*
%token       <string Region.reg Region.reg> Lang     "[%lang"
 *)

  (* Symbols *)

%token <string Wrap.wrap> MINUS   "-" [@recover.expr Wrap.ghost "-"]
%token <string Wrap.wrap> PLUS    "+" [@recover.expr Wrap.ghost "+"]
%token <string Wrap.wrap> SLASH   "/" [@recover.expr Wrap.ghost "/"]
%token <string Wrap.wrap> TIMES   "*" [@recover.expr Wrap.ghost "*"]
%token <string Wrap.wrap> REM     "%" [@recover.expr Wrap.ghost "%"]
(* %token <string Wrap.wrap> PLUS2   "++"*)
(* %token <string Wrap.wrap> MINUS2  "--"*)

%token <string Wrap.wrap> LPAR     "(" [@recover.expr Wrap.ghost "("]
%token <string Wrap.wrap> RPAR     ")" [@recover.expr Wrap.ghost ")"]
%token <string Wrap.wrap> LBRACKET "[" [@recover.expr Wrap.ghost "["]
%token <string Wrap.wrap> RBRACKET "]" [@recover.expr Wrap.ghost "]"]
%token <string Wrap.wrap> LBRACE   "{" [@recover.expr Wrap.ghost "{"]
%token <string Wrap.wrap> RBRACE   "}" [@recover.expr Wrap.ghost "}"]

%token <string Wrap.wrap> COMMA     "," [@recover.expr Wrap.ghost ","]
%token <string Wrap.wrap> SEMI      ";" [@recover.expr Wrap.ghost ";"]
%token <string Wrap.wrap> COLON     ":" [@recover.expr Wrap.ghost ":"]
%token <string Wrap.wrap> DOT       "." [@recover.expr Wrap.ghost "."]
%token <string Wrap.wrap> ELLIPSIS  "..." [@recover.expr Wrap.ghost "..."]

%token <string Wrap.wrap> BOOL_OR  "||" [@recover.expr Wrap.ghost "||"]
%token <string Wrap.wrap> BOOL_AND "&&" [@recover.expr Wrap.ghost "&&"]
%token <string Wrap.wrap> BOOL_NOT "!" [@recover.expr Wrap.ghost "!"]

// %token <string Wrap.wrap> BIT_AND  "&"
// %token <string Wrap.wrap> BIT_NOT  "~"
// %token <string Wrap.wrap> BIT_XOR  "^"
// %token <string Wrap.wrap> SHIFT_L  "<<<"
// %token <string Wrap.wrap> SHIFT_R  ">>>"

%token <string Wrap.wrap> EQ    "=" [@recover.expr Wrap.ghost "="]
%token <string Wrap.wrap> EQ2   "==" [@recover.expr Wrap.ghost "=="]
%token <string Wrap.wrap> NE    "!=" [@recover.expr Wrap.ghost "!="]

%token <string Wrap.wrap> LT    "<" [@recover.expr Wrap.ghost "<"]
%token <string Wrap.wrap> GT    ">" [@recover.expr Wrap.ghost ">"]
%token <string Wrap.wrap> LE    "<=" [@recover.expr Wrap.ghost "<="]
%token <string Wrap.wrap> GE    ">=" [@recover.expr Wrap.ghost ">="]

%token <string Wrap.wrap> PLUS_EQ  "+=" [@recover.expr Wrap.ghost "+="]
%token <string Wrap.wrap> MINUS_EQ "-=" [@recover.expr Wrap.ghost "-="]
%token <string Wrap.wrap> MULT_EQ  "*=" [@recover.expr Wrap.ghost "*="]
%token <string Wrap.wrap> REM_EQ   "%=" [@recover.expr Wrap.ghost "%="]
%token <string Wrap.wrap> DIV_EQ   "/=" [@recover.expr Wrap.ghost "/="]
// %token <string Wrap.wrap> SL_EQ    "<<<="
// %token <string Wrap.wrap> SR_EQ    ">>>="
// %token <string Wrap.wrap> AND_EQ   "&="
// %token <string Wrap.wrap> OR_EQ    "|="
// %token <string Wrap.wrap> XOR_EQ   "^="

%token <string Wrap.wrap> VBAR   "|" [@recover.expr Wrap.ghost "|"]
%token <string Wrap.wrap> ARROW  "=>" [@recover.expr Wrap.ghost "=>"]
%token <string Wrap.wrap> WILD   "_" [@recover.expr Wrap.ghost "_"]


(* JavaScript Keywords *)

%token <string Wrap.wrap> Case     "case" [@recover.expr Wrap.ghost "case"]
%token <string Wrap.wrap> Const    "const" [@recover.expr Wrap.ghost "const"]
%token <string Wrap.wrap> Default  "default" [@recover.expr Wrap.ghost "default"]
%token <string Wrap.wrap> Else     "else" [@recover.expr Wrap.ghost "else"]
%token <string Wrap.wrap> Export   "export" [@recover.expr Wrap.ghost "export"]
%token <string Wrap.wrap> For      "for" [@recover.expr Wrap.ghost "for"]
%token <string Wrap.wrap> If       "if" [@recover.expr Wrap.ghost "if"]
%token <string Wrap.wrap> Import   "import" [@recover.expr Wrap.ghost "import"]
%token <string Wrap.wrap> Let      "let" [@recover.expr Wrap.ghost "let"]
%token <string Wrap.wrap> Of       "of" [@recover.expr Wrap.ghost "of"]
%token <string Wrap.wrap> Return   "return" [@recover.expr Wrap.ghost "return"]
%token <string Wrap.wrap> Break    "break"  [@recover.expr Wrap.ghost "break"]
%token <string Wrap.wrap> Switch   "switch" [@recover.expr Wrap.ghost "switch"]
%token <string Wrap.wrap> While    "while" [@recover.expr Wrap.ghost "while"]

(* TypeScript keywords *)

%token <string Wrap.wrap> As        "as" [@recover.expr Wrap.ghost "as"]
%token <string Wrap.wrap> Namespace "namespace" [@recover.expr Wrap.ghost "namespace"]
%token <string Wrap.wrap> Type      "type" [@recover.expr Wrap.ghost "type"]

(* Virtual tokens *)

%token <string Wrap.wrap> ZWSP [@recover.expr Wrap.ghost ""]

(* End of File *)

%token <string Wrap.wrap> EOF [@recover.expr Wrap.ghost ""]

%%
