(* Grammar for boolean expressions in preprocessing directives *)

(* Tokens *)

%token <string> Ident "<ident>"
%token True "true"
%token False "false"
%token OR "||"
%token AND "&&"
%token EQ "=="
%token NEQ "!="
%token NOT "!"
%token LPAR "("
%token RPAR ")"
%token EOL

%{
(* START OF HEADER *)

let bin op arg1 arg2 env = op (arg1 env) (arg2 env)
let (<@) f g x = f (g x)

(* END OF HEADER *)
%}

(* Entries *)

%start expr
%type <Env.t -> State.mode> expr

%%

(* Grammar *)

expr:
  or_expr_level EOL {
    fun env -> State.(if $1 env then Copy else Skip) }

or_expr_level:
  or_expr_level "||" and_expr_level    { bin (||) $1 $3 }
| and_expr_level                       { $1            }

and_expr_level:
  and_expr_level "&&" unary_expr_level { bin (&&) $1 $3 }
| eq_expr_level                        { $1            }

eq_expr_level:
  eq_expr_level "==" unary_expr_level  { bin (=)  $1 $3 }
| eq_expr_level "!=" unary_expr_level  { bin (<>) $1 $3 }
| unary_expr_level                     { $1             }

unary_expr_level:
  "!" unary_expr_level                 { not <@ $2 }
| core_expr                            { $1       }

core_expr:
  "true"                               { fun _ -> true  }
| "false"                              { fun _ -> false }
| "<ident>"                            { Env.mem $1   }
| "(" or_expr_level ")"                { $2           }
