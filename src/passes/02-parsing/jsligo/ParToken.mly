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
(* Tokens (mirroring those defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>" [@recover.expr Linemarker (Region.wrap_ghost (0, "<invalid-path>", None)) ]
%token                  <string Wrap.wrap> BlockCom "<block_comment>" [@recover.expr wrap_ghost "<invalid-block-comment>"]
%token                  <string Wrap.wrap> LineCom  "<line_comment>" [@recover.expr wrap_ghost "<invalid-line-comment>"]
%token                  <string Wrap.wrap> String   "<string>" [@recover.expr wrap_ghost "<invalid-string-literal>"]
%token                  <string Wrap.wrap> Verbatim "<verbatim>" [@recover.expr wrap_ghost "<invalid-verbatim-literal>"]
%token        <(string * Hex.t) Wrap.wrap> Bytes    "<bytes>" [@recover.expr wrap_ghost ("<invalid-bytes-literal>", `Hex "")]
%token          <(string * Z.t) Wrap.wrap> Int      "<int>" [@recover.expr wrap_ghost ("<invalid-int-literal>", Z.zero)]
(*
%token          <(string * Z.t) Wrap.wrap> Nat      "<nat>"
%token      <(string * Int64.t) Wrap.wrap> Mutez    "<mutez>"
*)
%token                  <string Wrap.wrap> Ident    "<ident>" [@recover.expr wrap_ghost "<invalid-ident>"]
%token                  <string Wrap.wrap> UIdent   "<uident>" [@recover.expr wrap_ghost "<invalid-uident>"]
%token                 <Attr.t Region.reg> Attr     "[@attr]" [@recover.expr wrap_ghost "<invalid-attr-literal>"]
(*
%token       <string Region.reg Region.reg> Lang     "[%lang"
 *)

  (* Symbols *)

%token <string Wrap.wrap> MINUS   "-" [@recover.expr wrap_ghost "-"]
%token <string Wrap.wrap> PLUS    "+" [@recover.expr wrap_ghost "+"]
%token <string Wrap.wrap> SLASH   "/" [@recover.expr wrap_ghost "/"]
%token <string Wrap.wrap> TIMES   "*" [@recover.expr wrap_ghost "*"]
%token <string Wrap.wrap> REM     "%" [@recover.expr wrap_ghost "%"]
(* %token <string Wrap.wrap> PLUS2   "++"*)
(* %token <string Wrap.wrap> MINUS2  "--"*)

%token <string Wrap.wrap> LPAR     "(" [@recover.expr wrap_ghost "("]
%token <string Wrap.wrap> RPAR     ")" [@recover.expr wrap_ghost ")"]
%token <string Wrap.wrap> LBRACKET "[" [@recover.expr wrap_ghost "["]
%token <string Wrap.wrap> RBRACKET "]" [@recover.expr wrap_ghost "]"]
%token <string Wrap.wrap> LBRACE   "{" [@recover.expr wrap_ghost "{"]
%token <string Wrap.wrap> RBRACE   "}" [@recover.expr wrap_ghost "}"]

%token <string Wrap.wrap> COMMA     "," [@recover.expr wrap_ghost ","]
%token <string Wrap.wrap> SEMI      ";" [@recover.expr wrap_ghost ";"]
%token <string Wrap.wrap> COLON     ":" [@recover.expr wrap_ghost ":"]
%token <string Wrap.wrap> DOT       "." [@recover.expr wrap_ghost "."]
%token <string Wrap.wrap> ELLIPSIS  "..." [@recover.expr wrap_ghost "..."]

%token <string Wrap.wrap> BOOL_OR  "||" [@recover.expr wrap_ghost "||"]
%token <string Wrap.wrap> BOOL_AND "&&" [@recover.expr wrap_ghost "&&"]
%token <string Wrap.wrap> BOOL_NOT "!" [@recover.expr wrap_ghost "!"]

// %token <string Wrap.wrap> BIT_AND  "&"
// %token <string Wrap.wrap> BIT_NOT  "~"
// %token <string Wrap.wrap> BIT_XOR  "^"
// %token <string Wrap.wrap> SHIFT_L  "<<<"
// %token <string Wrap.wrap> SHIFT_R  ">>>"

%token <string Wrap.wrap> EQ    "=" [@recover.expr wrap_ghost "="]
%token <string Wrap.wrap> EQ2   "==" [@recover.expr wrap_ghost "=="]
%token <string Wrap.wrap> NE    "!=" [@recover.expr wrap_ghost "!="]

%token <string Wrap.wrap> LT    "<" [@recover.expr wrap_ghost "<"]
%token <string Wrap.wrap> GT    ">" [@recover.expr wrap_ghost ">"]
%token <string Wrap.wrap> LE    "<=" [@recover.expr wrap_ghost "<="]
%token <string Wrap.wrap> GE    ">=" [@recover.expr wrap_ghost ">="]

%token <string Wrap.wrap> PLUS_EQ  "+=" [@recover.expr wrap_ghost "+="]
%token <string Wrap.wrap> MINUS_EQ "-=" [@recover.expr wrap_ghost "-="]
%token <string Wrap.wrap> MULT_EQ  "*=" [@recover.expr wrap_ghost "*="]
%token <string Wrap.wrap> REM_EQ   "%=" [@recover.expr wrap_ghost "%="]
%token <string Wrap.wrap> DIV_EQ   "/=" [@recover.expr wrap_ghost "/="]
// %token <string Wrap.wrap> SL_EQ    "<<<="
// %token <string Wrap.wrap> SR_EQ    ">>>="
// %token <string Wrap.wrap> AND_EQ   "&="
// %token <string Wrap.wrap> OR_EQ    "|="
// %token <string Wrap.wrap> XOR_EQ   "^="

%token <string Wrap.wrap> VBAR   "|" [@recover.expr wrap_ghost "|"]
%token <string Wrap.wrap> ARROW  "=>" [@recover.expr wrap_ghost "=>"]
%token <string Wrap.wrap> WILD   "_" [@recover.expr wrap_ghost "_"]


(* JavaScript Keywords *)

%token <string Wrap.wrap> Case     "case" [@recover.expr wrap_ghost "case"]
%token <string Wrap.wrap> Const    "const" [@recover.expr wrap_ghost "const"]
%token <string Wrap.wrap> Default  "default" [@recover.expr wrap_ghost "default"]
%token <string Wrap.wrap> Else     "else" [@recover.expr wrap_ghost "else"]
%token <string Wrap.wrap> Export   "export" [@recover.expr wrap_ghost "export"]
%token <string Wrap.wrap> For      "for" [@recover.expr wrap_ghost "for"]
%token <string Wrap.wrap> If       "if" [@recover.expr wrap_ghost "if"]
%token <string Wrap.wrap> Import   "import" [@recover.expr wrap_ghost "import"]
%token <string Wrap.wrap> Let      "let" [@recover.expr wrap_ghost "let"]
%token <string Wrap.wrap> Of       "of" [@recover.expr wrap_ghost "of"]
%token <string Wrap.wrap> Return   "return" [@recover.expr wrap_ghost "return"]
%token <string Wrap.wrap> Break    "break"  [@recover.expr wrap_ghost "break"]
%token <string Wrap.wrap> Switch   "switch" [@recover.expr wrap_ghost "switch"]
%token <string Wrap.wrap> While    "while" [@recover.expr wrap_ghost "while"]

(* TypeScript keywords *)

%token <string Wrap.wrap> As        "as" [@recover.expr wrap_ghost "as"]
%token <string Wrap.wrap> Namespace "namespace" [@recover.expr wrap_ghost "namespace"]
%token <string Wrap.wrap> Type      "type" [@recover.expr wrap_ghost "type"]

(* Virtual tokens *)

%token <string Wrap.wrap> ZWSP [@recover.expr wrap_ghost ""]

(* End of File *)

%token <string Wrap.wrap> EOF [@recover.expr wrap_ghost ""]

%%
