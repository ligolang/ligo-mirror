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

  let default_reg = ref Region.ghost

  let mk partial = partial !default_reg

  let mk_Directive () =
    let value = (1, "file_path", None)
    and region = !default_reg
    in Directive.Linemarker Region.{value; region}

  let mk_lang () =
    let region = !default_reg in
    Region.{value = {value = "Ghost_lang"; region}; region}

  (* Ghost semantic values for inserted tokens *)

  let mk_string   () = mk (Token.wrap_string   "ghost string")
  let mk_verbatim () = mk (Token.wrap_verbatim "ghost verbatim")
  let mk_bytes    () = mk (Token.wrap_bytes    (Hex.of_string "Ghost bytes"))
  let mk_int      () = mk (Token.wrap_int      Z.zero)
  let mk_nat      () = mk (Token.wrap_nat      Z.zero)
  let mk_mutez    () = mk (Token.wrap_mutez    Int64.zero)
  let mk_ident    () = mk (Token.wrap_ident    "ghost_ident")
  let mk_uident   () = mk (Token.wrap_uident   "Ghost_uident")
  let mk_attr     () = mk (Token.wrap_attr     "ghost_attr" None)
]

(* Literals *)

%token         <LexerLib.Directive.t> Directive "<directive>" [@recover.expr mk_Directive ()]
%token                <string Wrap.t> String    "<string>"    [@recover.expr mk_string    ()]
%token                <string Wrap.t> Verbatim  "<verbatim>"  [@recover.expr mk_verbatim  ()]
%token      <(string * Hex.t) Wrap.t> Bytes     "<bytes>"     [@recover.expr mk_bytes     ()]
%token        <(string * Z.t) Wrap.t> Int       "<int>"       [@recover.expr mk_int       ()]
%token        <(string * Z.t) Wrap.t> Nat       "<nat>"       [@recover.expr mk_nat       ()]
%token    <(string * Int64.t) Wrap.t> Mutez     "<mutez>"     [@recover.expr mk_mutez     ()]
%token                <string Wrap.t> Ident     "<ident>"     [@recover.expr mk_ident     ()]
%token                <string Wrap.t> UIdent    "<uident>"    [@recover.expr mk_uident    ()]
%token            <Attr.t Region.reg> Attr      "[@attr]"     [@recover.expr mk_attr      ()]
%token <string Region.reg Region.reg> Lang      "[%lang"      [@recover.expr mk_lang      ()]

(* Symbols *)

%token <string Wrap.t> SEMI     ";"   [@recover.expr mk Token.wrap_semi]
%token <string Wrap.t> COMMA    ","   [@recover.expr mk Token.wrap_comma]
%token <string Wrap.t> LPAR     "("   [@recover.expr mk Token.wrap_lpar]
%token <string Wrap.t> RPAR     ")"   [@recover.expr mk Token.wrap_rpar]
%token <string Wrap.t> LBRACE   "{"   [@recover.expr mk Token.wrap_lbrace]
%token <string Wrap.t> RBRACE   "}"   [@recover.expr mk Token.wrap_rbrace]
%token <string Wrap.t> LBRACKET "["   [@recover.expr mk Token.wrap_lbracket]
%token <string Wrap.t> RBRACKET "]"   [@recover.expr mk Token.wrap_rbracket]
%token <string Wrap.t> SHARP    "#"   [@recover.expr mk Token.wrap_sharp]
%token <string Wrap.t> VBAR     "|"   [@recover.expr mk Token.wrap_vbar]
%token <string Wrap.t> ARROW    "->"  [@recover.expr mk Token.wrap_arrow]
%token <string Wrap.t> ASS      ":="  [@recover.expr mk Token.wrap_ass]
%token <string Wrap.t> EQ       "="   [@recover.expr mk Token.wrap_eq]
%token <string Wrap.t> COLON    ":"   [@recover.expr mk Token.wrap_colon]
%token <string Wrap.t> LT       "<"   [@recover.expr mk Token.wrap_lt]
%token <string Wrap.t> LE       "<="  [@recover.expr mk Token.wrap_le]
%token <string Wrap.t> GT       ">"   [@recover.expr mk Token.wrap_gt]
%token <string Wrap.t> GE       ">="  [@recover.expr mk Token.wrap_ge]
%token <string Wrap.t> NE       "=/=" [@recover.expr mk Token.wrap_ne]
%token <string Wrap.t> PLUS     "+"   [@recover.expr mk Token.wrap_plus]
%token <string Wrap.t> MINUS    "-"   [@recover.expr mk Token.wrap_minus]
%token <string Wrap.t> SLASH    "/"   [@recover.expr mk Token.wrap_slash]
%token <string Wrap.t> TIMES    "*"   [@recover.expr mk Token.wrap_times]
%token <string Wrap.t> DOT      "."   [@recover.expr mk Token.wrap_dot]
%token <string Wrap.t> WILD     "_"   [@recover.expr mk Token.wrap_wild]
%token <string Wrap.t> CARET    "^"   [@recover.expr mk Token.wrap_caret]

(* Keywords *)

%token <string Wrap.t> And       "and"       [@recover.expr mk Token.wrap_and]
%token <string Wrap.t> Begin     "begin"     [@recover.expr mk Token.wrap_begin]
%token <string Wrap.t> BigMap    "big_map"   [@recover.expr mk Token.wrap_big_map]
%token <string Wrap.t> Block     "block"     [@recover.expr mk Token.wrap_block]
%token <string Wrap.t> Case      "case"      [@recover.expr mk Token.wrap_case]
%token <string Wrap.t> Const     "const"     [@recover.expr mk Token.wrap_const]
%token <string Wrap.t> Contains  "contains"  [@recover.expr mk Token.wrap_contains]
%token <string Wrap.t> Else      "else"      [@recover.expr mk Token.wrap_else]
%token <string Wrap.t> End       "end"       [@recover.expr mk Token.wrap_end]
%token <string Wrap.t> For       "for"       [@recover.expr mk Token.wrap_for]
%token <string Wrap.t> Function  "function"  [@recover.expr mk Token.wrap_function]
%token <string Wrap.t> Recursive "recursive" [@recover.expr mk Token.wrap_recursive]
%token <string Wrap.t> From      "from"      [@recover.expr mk Token.wrap_from]
%token <string Wrap.t> If        "if"        [@recover.expr mk Token.wrap_if]
%token <string Wrap.t> In        "in"        [@recover.expr mk Token.wrap_in]
%token <string Wrap.t> Is        "is"        [@recover.expr mk Token.wrap_is]
%token <string Wrap.t> List      "list"      [@recover.expr mk Token.wrap_list]
%token <string Wrap.t> Map       "map"       [@recover.expr mk Token.wrap_map]
%token <string Wrap.t> Mod       "mod"       [@recover.expr mk Token.wrap_mod]
%token <string Wrap.t> Nil       "nil"       [@recover.expr mk Token.wrap_nil]
%token <string Wrap.t> Not       "not"       [@recover.expr mk Token.wrap_not]
%token <string Wrap.t> Of        "of"        [@recover.expr mk Token.wrap_of]
%token <string Wrap.t> Or        "or"        [@recover.expr mk Token.wrap_or]
%token <string Wrap.t> Patch     "patch"     [@recover.expr mk Token.wrap_patch]
%token <string Wrap.t> Record    "record"    [@recover.expr mk Token.wrap_record]
%token <string Wrap.t> Remove    "remove"    [@recover.expr mk Token.wrap_remove]
%token <string Wrap.t> Set       "set"       [@recover.expr mk Token.wrap_set]
%token <string Wrap.t> Skip      "skip"      [@recover.expr mk Token.wrap_skip]
%token <string Wrap.t> Step      "step"      [@recover.expr mk Token.wrap_step]
%token <string Wrap.t> Then      "then"      [@recover.expr mk Token.wrap_then]
%token <string Wrap.t> To        "to"        [@recover.expr mk Token.wrap_to]
%token <string Wrap.t> Type      "type"      [@recover.expr mk Token.wrap_type]
%token <string Wrap.t> Var       "var"       [@recover.expr mk Token.wrap_var]
%token <string Wrap.t> While     "while"     [@recover.expr mk Token.wrap_while]
%token <string Wrap.t> With      "with"      [@recover.expr mk Token.wrap_with]
%token <string Wrap.t> Module    "module"    [@recover.expr mk Token.wrap_module]

(* End of File *)

%token <string Wrap.wrap> EOF [@recover.expr mk Token.wrap_eof]

%%
