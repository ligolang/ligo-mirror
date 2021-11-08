%{
module Token = Lexing_pascaligo.Token
%}

(* Tokens (mirroring thise defined in module Token) *)

  (* Literals *)

%token               <LexerLib.Directive.t> Directive "<directive>"
%token            <Token.lexeme Token.wrap> String    "<string>"
%token            <Token.lexeme Token.wrap> Verbatim  "<verbatim>"
%token  <(Token.lexeme * Hex.t) Token.wrap> Bytes     "<bytes>"
%token    <(Token.lexeme * Z.t) Token.wrap> Int       "<int>"
%token    <(Token.lexeme * Z.t) Token.wrap> Nat       "<nat>"
%token    <(Token.lexeme * Z.t) Token.wrap> Mutez     "<mutez>"
%token            <Token.lexeme Token.wrap> Ident     "<ident>"
%token            <Token.lexeme Token.wrap> UIdent    "<uident>"
%token                  <string Token.wrap> Attr      "[@attr]"
%token <Token.lexeme Region.reg Region.reg> Lang      "[%lang"

  (* Symbols *)

%token <lexeme Wrap.t> SEMI        ";"
%token <lexeme Wrap.t> COMMA       ","
%token <lexeme Wrap.t> LPAR        "("
%token <lexeme Wrap.t> RPAR        ")"
%token <lexeme Wrap.t> LBRACE      "{"
%token <lexeme Wrap.t> RBRACE      "}"
%token <lexeme Wrap.t> LBRACKET    "["
%token <lexeme Wrap.t> RBRACKET    "]"
%token <lexeme Wrap.t> SHARP       "#"
%token <lexeme Wrap.t> VBAR        "|"
%token <lexeme Wrap.t> ARROW       "->"
%token <lexeme Wrap.t> ASS         ":="
%token <lexeme Wrap.t> EQ          "="
%token <lexeme Wrap.t> COLON       ":"
%token <lexeme Wrap.t> LT          "<"
%token <lexeme Wrap.t> LE          "<="
%token <lexeme Wrap.t> GT          ">"
%token <lexeme Wrap.t> GE          ">="
%token <lexeme Wrap.t> NE          "=/="
%token <lexeme Wrap.t> PLUS        "+"
%token <lexeme Wrap.t> MINUS       "-"
%token <lexeme Wrap.t> SLASH       "/"
%token <lexeme Wrap.t> TIMES       "*"
%token <lexeme Wrap.t> DOT         "."
%token <lexeme Wrap.t> WILD        "_"
%token <lexeme Wrap.t> CARET       "^"

  (* Keywords *)

%token <lexeme Wrap.t> And         "and"
%token <lexeme Wrap.t> Begin       "begin"
%token <lexeme Wrap.t> BigMap      "big_map"
%token <lexeme Wrap.t> Block       "block"
%token <lexeme Wrap.t> Case        "case"
%token <lexeme Wrap.t> Const       "const"
%token <lexeme Wrap.t> Contains    "contains"
%token <lexeme Wrap.t> Else        "else"
%token <lexeme Wrap.t> End         "end"
%token <lexeme Wrap.t> For         "for"
%token <lexeme Wrap.t> Function    "function"
%token <lexeme Wrap.t> Recursive   "recursive"
%token <lexeme Wrap.t> From        "from"
%token <lexeme Wrap.t> If          "if"
%token <lexeme Wrap.t> In          "in"
%token <lexeme Wrap.t> Is          "is"
%token <lexeme Wrap.t> List        "list"
%token <lexeme Wrap.t> Map         "map"
%token <lexeme Wrap.t> Mod         "mod"
%token <lexeme Wrap.t> Nil         "nil"
%token <lexeme Wrap.t> Not         "not"
%token <lexeme Wrap.t> Of          "of"
%token <lexeme Wrap.t> Or          "or"
%token <lexeme Wrap.t> Patch       "patch"
%token <lexeme Wrap.t> Record      "record"
%token <lexeme Wrap.t> Remove      "remove"
%token <lexeme Wrap.t> Set         "set"
%token <lexeme Wrap.t> Skip        "skip"
%token <lexeme Wrap.t> Step        "step"
%token <lexeme Wrap.t> Then        "then"
%token <lexeme Wrap.t> To          "to"
%token <lexeme Wrap.t> Type        "type"
%token <lexeme Wrap.t> Var         "var"
%token <lexeme Wrap.t> While       "while"
%token <lexeme Wrap.t> With        "with"
%token <lexeme Wrap.t> Module      "module"

  (* Virtual tokens *)

%token <lexeme Wrap.t> EOF

%%
