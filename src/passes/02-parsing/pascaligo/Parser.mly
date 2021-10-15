(* Menhir specification of the parser of PascaLIGO

   "Beaucoup de mal durable est souvent fait par les choses provisoires."
                                              Victor Hugo, 11 Sept. 1848
                         http://classes.bnf.fr/laicite/anthologie/32.htm

   About Menhir:
     http://gallium.inria.fr/blog/parser-construction-menhir-appetizers/
     http://gallium.inria.fr/~fpottier/menhir/manual.pdf

   This grammar specification actually encompasses two dialects of
   PascaLIGO. One is called _verbose_ because it uses quite a number
   of keywords, and the other one is called _terse_ because it
   requires fewer keywords. Generally speaking, the latter replaces
   many keywords 'end' by using symbols instead. An example of verbose
   record type definition would be:

     type t is record a : int end

   The terse version is

     type t is record [a : int]

   This grammar does not enforce one dialect over the other. We always
   present the terse version before the verbose one, in each rule
   where the distinction applies.

   When laying out rules for the same non-terminal, we use the closing
   brace of a rule to separate it from the next by being on its own
   line, like so:

     foo:
       .... { ...
       }
     | ... { ... }

   When there are many rules for the same terminal, we present the
   rules for the non-terminals involved in a left-right prefix manner
   (a.k.a depth-first traversal in an algorithmic context). For
   example:

     foo:
       bar { ... }
     | baz { ... }

     bar:
       zoo { ... }

     zoo:
       A { ... }

     baz:
       B { ... }

   When you change the grammar, take some time to see if you cannot
   remove a reduction on error that is related to your change.

   Write comments. Inside them, escape text by writing it between
   square brackets, following the ocamldoc convention.

   Please avoid writing a leading vertical bar, like

     foo:
       | bar {}

   The above is equivalent to

     foo:
       bar {}

   but people could think it means

     for:
       {} | bar {}

   because Menhir enables the sharing of semantic actions. (By the
   way, the leading vertical bar is the only cause of an LR conflict
   in the grammar of Menhir itself (personal communication to
   Rinderknecht by Pottier, June 23, 2006).

   We do not rely on predefined Menhir symbols, like $symbolstartpos,
   to help determine the regions (that is, source locations) of our
   tokens and subtrees of our CST. One reason is that the semantic of
   $symbolstartpos is that of ocamlyacc, and this does not blend well
   with nullable prefixes of rules. That is why we use [Region.cover]
   to compute the region in the source that corresponds to any given
   CST node. This is more verbose than letting Menhir ask the lexer
   buffer with a surprising semantic, but we are 100% in control.

   A note on terminology: I sometimes use words taken from the context
   of formal logic or programming theory. For example:
   https://en.wikipedia.org/wiki/Extensional_and_intensional_definitions

   We always define and write values of type [Region.t] by stating the
   region first, like so: [{region; value}], and we always use name
   punning. *)

%{
(* START HEADER *)

[@@@warning "-42"]

(* Dependencies *)

open Simple_utils.Region
module CST = Cst_pascaligo.CST
open CST

(* UTILITIES

   The following functions help build CST nodes. When they are
   complicated, like [mk_mod_path], it is because the grammar rule had
   to be written in a certain way to remain LR, and that way did not
   make it easy in the semantic action to collate the information into
   CST nodes. *)

let mk_set     region = `Set    region
let mk_list    region = `List   region
let mk_map     region = `Map    region
let mk_big_map region = `BigMap region
let mk_record  region = `Record region
let mk_wild    region = Region.{value="_"; region}

let mk_reg  region value = Region.{region; value}
let mk_EVar region value = E_Var Region.{region; value}

let mk_wild_attr region =
  let variable = {value="_"; region} in
  let value = {variable; attributes=[]}
  in {region; value}

let list_of_option = function
       None -> []
| Some list -> list

let mk_mod_path :
  (module_name * dot) Utils.nseq * 'a ->
  ('a -> Region.t) ->
  'a CST.module_path Region.reg =
  fun (nseq, field) to_region ->
    let (first, sep), tail = nseq in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item, next_sep) :: others ->
        trans ((prev_sep, item) :: seq, next_sep) others in
    let list, last_dot = trans ([], sep) tail in
    let module_path = first, List.rev list in
    let region = CST.nseq_to_region (fun (x,_) -> x.region) nseq in
    let region = Region.cover region (to_region field)
    and value = {module_path; selector=last_dot; field}
    in {value; region}

(* The function [terminate_decl] adds a semicolon terminator to a
   declaration. It is a functional update because we want to share
   rules for declarations that are valid at top-level and at inner
   levels (blocks), and the latter require a semicolon --- which we
   patch afterwards in semantic actions. *)

let terminate_decl semi = function
  D_Const     d -> {d with value = {d.value with terminator=semi}}
| D_Directive d -> d
| D_Fun       d -> {d with value = {d.value with terminator=semi}}
| D_Module    d -> {d with value = {d.value with terminator=semi}}
| D_ModAlias  d -> {d with value = {d.value with terminator=semi}}
| D_Type      d -> {d with value = {d.value with terminator=semi}}

(* END HEADER *)
%}

(* See [ParToken.mly] for the definition of tokens. *)

(* Entry points *)

%start contract interactive_expr
%type <CST.t> contract
%type <CST.expr> interactive_expr

(* Reductions on error states is sometimes needed in order to end on a
   state where there is hopefully more right context to provide better
   error messages about possible futures. Practically, if, when
   examining the syntax error messages file, one comes accros an LR
   item of the form

     X -> some Things . [SOME TOKENS]

   where the next tokens between the square brackets are numerous
   and/or diverse in nature, then it is a good idea to add a clause

   %on_error_reduce X

   This will instruct the run-time generated by Menhir to reduce after
   the error, in the hopes to find a state where it is easier to
   predict possible futures. One typical use of this pleasant feature
   is required by the design choice we made to mix two dialects of
   PascaLIGO in the same grammar, the so-called "terse" and "verbose"
   dialects. One difference between both is the way compound
   constructs are closed. In the verbose dialect, the keyword "end" is
   used, instead of a symbol in the terse dialect. This yields error
   states of the form

      some_compound_construct -> ... . [End RBRACKET]

   In that state, we do not know the exact future and we cannot
   confuse the user by suggesting the wrong dialect -- a user who might
   not even be aware of the existence of those dialects. Whence the

     %on_error_reduce some_compound_construct

  Beware that the same item that would benefit from a reduction on
  error may occur in different states of the underlying LR
  automaton. In that case, priority will have to be specified by order
  of writing. When not using priorities, it is normally advised to
  list all the sentences to reduce in the same %on_error_reduce, but
  we do not do so, which make it is hard to remember when priority
  played a role, because Menhir only reports the number of states where
  priority of reduction played a role, but does not tell which
  ones. In the PascaLIGO grammar, two states are concerned. To find
  which, we would need to rewrite the following clauses as one until
  Menhir warns of the need of a priority specification. Unfortunately,
  the algorithm in Menhir for --list-errors is currently very slow, so
  this kind of refactoring would be very costly to do. A much faster
  algorithm will be in production in the near future, enabling that
  work to be carried out in a reasonable amount of time. *)

%on_error_reduce nseq(__anonymous_0(field_decl,SEMI))
%on_error_reduce nseq(__anonymous_0(field_path_assignment,SEMI))
%on_error_reduce nseq(__anonymous_0(binding,SEMI))
%on_error_reduce nseq(__anonymous_0(field_assignment,SEMI))
%on_error_reduce nseq(__anonymous_0(core_pattern,SEMI))
%on_error_reduce nseq(__anonymous_0(expr,SEMI))
%on_error_reduce nsepseq(field_assignment,SEMI)
%on_error_reduce nseq(__anonymous_0(statement,SEMI))
%on_error_reduce nsepseq(case_clause(expr),VBAR)
%on_error_reduce nsepseq(core_pattern,SEMI)
%on_error_reduce pattern
%on_error_reduce nsepseq(core_pattern,CONS)
%on_error_reduce nsepseq(case_clause(if_clause),VBAR)
%on_error_reduce lhs
%on_error_reduce map_lookup
%on_error_reduce nsepseq(statement,SEMI)
%on_error_reduce nsepseq(core_pattern,COMMA)
%on_error_reduce ctor_pattern
%on_error_reduce core_expr
%on_error_reduce nsepseq(param_decl,SEMI)
%on_error_reduce nsepseq(selection,DOT)
%on_error_reduce nsepseq(field_path_assignment,SEMI)
%on_error_reduce nsepseq(binding,SEMI)
%on_error_reduce nsepseq(expr,SEMI)
%on_error_reduce add_expr
%on_error_reduce unary_expr
%on_error_reduce const_decl
%on_error_reduce open_const_decl
%on_error_reduce fun_decl
%on_error_reduce variant(fun_type_level)
%on_error_reduce nsepseq(variant(fun_type_level),VBAR)
%on_error_reduce core_type
%on_error_reduce nsepseq(field_decl,SEMI)
%on_error_reduce nsepseq(core_type,TIMES)
%on_error_reduce type_decl
%on_error_reduce cartesian_level
%on_error_reduce fun_type_level
%on_error_reduce cons_expr
%on_error_reduce cat_expr
%on_error_reduce set_membership
%on_error_reduce disj_expr
%on_error_reduce core_pattern
%on_error_reduce nsepseq(type_expr,COMMA)
%on_error_reduce expr
%on_error_reduce nsepseq(expr,COMMA)
%on_error_reduce option(SEMI)
%on_error_reduce option(VBAR)
%on_error_reduce projection
%on_error_reduce nsepseq(module_name,DOT)
%on_error_reduce nseq(declaration)
%on_error_reduce option(arguments)
%on_error_reduce path
%on_error_reduce nseq(Attr)

%%

(* RULES *)

(* The rule [sep_or_term(item,sep)] ("separated or terminated list")
   parses a non-empty list of items separated by [sep], and optionally
   terminated by [sep]. The follwing rules were inspired by the
   following blog post by Pottier:

   http://gallium.inria.fr/blog/lr-lists/
*)

sep_or_term_list(item,sep):
  nsepseq(item,sep) {
    $1, None
  }
| nseq(item sep {$1,$2}) {
    let (first,sep), tail = $1 in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item,next_sep)::others ->
        trans ((prev_sep,item)::seq, next_sep) others in
    let list, term = trans ([],sep) tail
    in (first, List.rev list), Some term }

(* Compound constructs *)

par(X):
  "(" X ")" {
    let region = cover $1 $3
    and value  = {lpar=$1; inside=$2; rpar=$3}
    in {region; value} }

brackets(X):
  "[" X "]" {
    let region = cover $1 $3
    and value  = {lbracket=$1; inside=$2; rbracket=$3}
    in {region; value} }

(* Sequences

   Series of instances of the same syntactical category have often to
   be parsed, like lists of expressions, patterns etc. The non-empty
   sequence is parsed by [nseq], which returns a pair made of the
   first parsed item (the parameter [X]) and the rest of the sequence
   (possibly empty). This way, the OCaml typechecker can keep track of
   this information along the static control-flow graph. The rule
   [nsepseq] is for non-empty such sequences. See module [Utils] for
   the types corresponding to the semantic actions of those rules.  *)

(* Non-empty sequence of items *)

nseq(X):
  X         { $1, [] }
| X nseq(X) { let hd,tl = $2 in $1, hd::tl }

(* Non-empty separated sequence of items *)

nsepseq(X,Sep):
  X                    {                 $1,        [] }
| X Sep nsepseq(X,Sep) { let h,t = $3 in $1, ($2,h)::t }

(* Aliasing and inlining some tokens *)

%inline variable    : "<ident>"  { $1 }
%inline type_var    : "<ident>"  { $1 }
%inline type_name   : "<ident>"  { $1 }
%inline fun_name    : "<ident>"  { $1 }
%inline field_name  : "<ident>"  { $1 }
%inline struct_name : "<ident>"  { $1 }
%inline module_name : "<uident>" { $1 }
%inline ctor        : "<uident>" { $1 }

(* Unary operators *)

unary_op(op,arg):
  op arg {
    let region = cover $1 (expr_to_region $2)
    and value  = {op=$1; arg=$2}
    in {region; value} }

(* Binary operators *)

bin_op(arg1,op,arg2):
  arg1 op arg2 {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in {region; value} }

(* Attributes *)

%inline attributes:
  ioption(nseq("[@attr]") { Utils.nseq_to_list $1 }) { list_of_option $1 }

(* ENTRY *)

contract:
  nseq(top_declaration) EOF { {decl=$1; eof=$2} }

(* DECLARATIONS (top-level)

   Some declarations occur at the top level and others in
   blocks. They differ in what is allowed according to the
   context. For instance, at top-level, no "var" variable is allowed,
   but preprocessing directives (linemarkers from #include) are,
   whereas in a block it is the opposite. Also, at the top level,
   semicolons are optional when terminating declarations, whereas they
   are mandatory in blocks, because instructions can be mixed with
   them, providing no clear lexical indication as to where the next
   declaration starts, hence the semicolons. *)

top_declaration:
  declaration ";"? { terminate_decl $2 $1 }
| "<directive>"    { D_Directive       $1 } (* Only at top-level *)

declaration:
  type_decl    { D_Type     $1 }
| const_decl   { D_Const    $1 }
| fun_decl     { D_Fun      $1 }
| module_decl  { D_Module   $1 }
| module_alias { D_ModAlias $1 }

declarations:
  nseq(declaration ";"? { terminate_decl $2 $1}) { $1 }

(* Type declarations *)

type_decl:
  "type" type_name type_params? "is" type_expr {
    let stop   = type_expr_to_region $5 in
    let region = cover $1 stop in
    let value  = {kwd_type=$1; name=$2; params=$3; kwd_is=$4;
                  type_expr=$5; terminator=None}
    in {region; value} }

type_params:
  par(nsepseq(type_var,",")) { $1 }

(* Type expressions

   The following subgrammar is _stratified_ in the usual manner to
   build in the grammar the different priorities between the syntactic
   categories. This is the same technique often used to handle
   arithmetic and Boolean expressions, for instance, without resorting
   to Menhir annotations. For example, see [cartesian_level] where
   [core_type] cannot derive a cartesian type (that is, a product type
   that is not a record type).

     The rule [fun_type_level] enforces the right-associativity of the
   arrow type constructor because the rule is right-recursive. So "a
   -> b -> c" is parsed as "a -> (b -> c)". This is also a classic.

     Note also how [sum_type] is at the same level as
   [fun_type_level], so, for instance, the derivation of [t -> Foo] is
   not permitted. In that case, parentheses are needed: [t ->
   (Foo)]. If that case does not seem a good rationale, consider [t ->
   Foo of string * int]: is it [t -> Foo of (string * int)] or [t ->
   (Foo of string) * int]? In OCaml, the closest type expressions are
   polymorphic variants, and they require delimiters in this case,
   like so: [t -> [`Foo of string * int]]. Anyhow, this is why
   [sum_type] is parameterised. *)

type_expr:
  fun_type_level | sum_type(fun_type_level) { $1 }

fun_type_level:
  cartesian_level { $1 }
| cartesian_level "->" fun_type_level {
    let start  = type_expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop in
    T_Fun {region; value = $1,$2,$3} }

cartesian_level:
  core_type "*" nsepseq(core_type,"*") {
    let value  = Utils.nsepseq_cons $1 $2 $3 in
    let region = nsepseq_to_region type_expr_to_region value
    in T_Cart {region; value}
  }
| core_type { $1 }

core_type:
  "<string>"     { TString  $1 }
| "<int>"        { TInt     $1 }
| "_"            { TVar     (mk_wild $1) }
| type_name      { TVar     $1 }
| type_in_module { TModPath $1 }
| type_ctor_app  { TApp     $1 }
| record_type    { TRecord  $1 }
| par(type_expr) { TPar     $1 }

(* Type constructor applications

   We handle here "map", "big_map", "set" and "list" because they are
   keywords. *)

type_ctor_app:
  type_in_module type_tuple {
    let region = cover $1.region $2.region
    in mk_reg region ($1,$2)
  }
| "map" type_tuple {
    let region    = cover $1 $2.region in
    let type_ctor = mk_reg $1 "map"
    in mk_reg region (type_ctor, $2)
  }
| "big_map" type_tuple {
    let region    = cover $1 $2.region in
    let type_ctor = mk_reg $1 "big_map"
    in mk_reg region (type_ctor, $2)
  }
| "set" par(type_expr) {
    let total     = cover $1 $2.region in
    let type_ctor = mk_reg $1 "set" in
    let {region; value = {lpar; inside; rpar}} = $2 in
    let tuple = mk_reg region {lpar; inside=inside,[]; rpar}
    in mk_reg total (type_ctor, tuple)
  }
| "list" par(type_expr) {
    let total = cover $1 $2.region in
    let type_ctor = mk_reg $1 "list" in
    let {region; value = {lpar; inside; rpar}} = $2 in
    let tuple = mk_reg region {lpar; inside=inside,[]; rpar}
    in mk_reg total (type_ctor, tuple) }

type_tuple:
  par(nsepseq(type_expr,",")) { $1 }

(* Type qualifications

   The rule [module_path] is parameterised by what is derived after a
   series of selections of modules inside modules (nested modules),
   like [A.B.C.D]. For example, here, we want to qualify ("select") a
   type in a module, so the parameter is [type_name], because only
   types defined at top-level are in the scope (that is, any type
   declaration inside blocks is not). Then we can derive
   [A.B.C.D.t]. Notice that, in the semantic action of
   [type_in_module] we call the function [mk_mod_path] to reorganise
   the steps of the path and thus fit our CST. That complicated step
   is necessary because we need an LR(1) grammar. Indeed, rule
   [module_path] is right-recursive, yielding the reverse order of
   selection: "A.(B.(C))" instead of the expected "((A).B).C": the
   function [mk_mod_path] the semantic action of [type_in_module]
   reverses that path. We could have chosen to leave the associativity
   unspecified, like so:

     type_in_module:
       nsepseq(module_name,".") "." type_name { ... }

   Unfortunately, this creates a shift/reduce conflict (on "."),
   whence our more involved solution. *)

type_in_module:
  module_path(type_name) { mk_mod_path $1 (fun x -> x.region) }

module_path(selected):
  module_name "." module_path(selected) {
    let (head, tail), selected = $3 in
    (($1,$2), head::tail), selected
  }
| module_name "." selected { (($1,$2), []), $3 }

(* Sum type

   The syntax of PascaLIGO's sum types (a.k.a variant types) is
   inspired by OCaml, and shared as such with the grammar of
   CameLIGO. The reason is that Pascal has no real equivalent of sum
   types, beyond enumerated types.

     Note the position of the attributes that apply to the sum type,
   in the second rule [sum_type]: the attributes are first and must be
   followed by a vertical bar before the variants are laid
   out. Attributes about a specific variant is written _after_ the
   vertical bar introducing to its right the variant, as seen in rule
   [variant].

   We chose to distinguish the cases of constant (nullary)
   constructors, to keep the semantic actions shorter. *)

sum_type(right_type_expr):
  nsepseq(variant(right_type_expr),"|") {
    let region = nsepseq_to_region (fun x -> x.region) $1 in
    let value  = {variants=$1; attributes=[]; lead_vbar=None}
    in T_Sum {region; value}
  }
| attributes "|" nsepseq(variant(right_type_expr),"|") {
    let region = nsepseq_to_region (fun x -> x.region) $3 in
    let value  = {variants=$3; attributes=$1; lead_vbar = Some $2}
    in T_Sum {region; value} }

(* Reminder: Always use [ioption] at the end of a rule *)

variant(right_type_expr):
  attributes "<uident>" ioption(of_type(right_type_expr)) {
    let stop   = match $3 with
                   None -> $2.region
                | Some (_, t) -> type_expr_to_region t in
    let region = cover $2.region stop
    and value  = {ctor=$2; arg=$3; attributes=$1}
    in {region; value} }

of_type(right_type_expr):
  "of" right_type_expr { $1,$2 }

(* Record types *)

record_type:
  attributes injection("record",field_decl) {
    let region = match $1 with
                         [] -> $2.region
                 | start::_ -> cover start.region $2.region
    and value = {$2 with value = {$2.value with attributes=$1}}
    in {region; value} }

field_decl:
  attributes field_name ioption(type_annotation) {
    let stop = match $3 with
                        None -> $2.region
               | Some (_, t) -> type_expr_to_region t in
    let region = match $1 with
                         [] -> cover $2.region stop
                 | start::_ -> cover start.region stop in
    let value = {attributes=$1; field_name=$2; field_type=$3}
    in {region; value} }

type_annotation:
  ":" type_expr { $1,$2 }

(* Constant declarations *)

(* Note the use of the rule [unqualified_decl]. It takes a parameter
   that derives the kind of definitional symbol. In the case of
   constants, that symbol is the mathematical equality '='. The case
   of "var" variables, which are not allowed in top-level
   declarations, that symbol would be ':='. See [var_decl] below. *)

const_decl:
  attributes "const" unqualified_decl("=") {
    let pattern, const_type, equal, init, stop = $3 in
    let region = cover $2 stop
    and value  = {attributes=$1; kwd_const=$2; pattern; const_type;
                  equal; init; terminator=None}
    in {region; value} }

unqualified_decl(OP):
  core_pattern ioption(type_annotation) OP expr {
    let region = expr_to_region $4
    in $1, $2, $3, $4, region }

(* Function declarations *)

fun_decl:
  attributes ioption("recursive") "function" fun_name parameters
  ioption(type_annotation) "is" expr {
    let start  = match $2 with Some start -> start | None -> $3 in
    let stop   = expr_to_region $8 in
    let region = cover start stop
    and value  = {attributes=$1; kwd_recursive=$2; kwd_function=$3;
                  fun_name=$4; param=$5; ret_type=$6; kwd_is=$7;
                  return=$8; terminator=None}
    in {region; value} }

parameters:
  par(nsepseq(param_decl,";")) { $1 }

param_decl:
  param_kind var_or_wild param_type? {
    let stop   = match $3 with
                         None -> $2.region
                 | Some (_,t) -> type_expr_to_region t in
    let region = cover $1 stop
    and value  = {param_kind=$1; var=$2; param_type=$3}
    in {region; value} }

param_kind:
  "var"   { `Var   $1 }
| "const" { `Const $1 }

var_or_wild:
  var_pattern { $1              }
| "_"         { mk_wild_attr $1 }

param_type:
  ":" fun_type_level { $1,$2 }

(* Module declaration *)

(* The first rule [module_decl] is the terse version, whereas the the
   second is the verbose one. *)

module_decl:
  terse_module_decl | verb_module_decl { $1 }

terse_module_decl:
  "module" module_name "is" "block"? "{" declarations "}" {
    let enclosing = Braces ($4,$5,$7) in
    let region    = cover $1 $7
    and value     = {kwd_module=$1; name=$2; kwd_is=$3; enclosing;
                     declarations=$6; terminator=None}
    in {region; value} }

verb_module_decl:
  "module" module_name "is" "begin" declarations "end" {
    let enclosing = BeginEnd ($4,$6) in
    let region    = cover $1 $6
    and value     = {kwd_module=$1; name=$2; kwd_is=$3; enclosing;
                     declarations=$5; terminator=None}
    in {region; value} }

(* Module aliases *)

module_alias:
  "module" module_name "is" nsepseq(module_name,".") {
    let stop   = nsepseq_to_region (fun x -> x.region) $4 in
    let region = cover $1 stop in
    let value  = {kwd_module=$1; alias=$2; kwd_is=$3;
                  mod_path=$4; terminator=None}
    in {region; value} }

(* STATEMENTS *)

statement:
  declaration { S_Decl    $1 }
| var_decl    { S_VarDecl $1 } (* Not allowed at top-level *)
| instruction { S_Instr   $1 } (* Not allowed at top-level *)

(* XXX *)

fun_expr:
  attributes "function" parameters ioption(type_annotation) "is" expr {
    let stop   = expr_to_region $6 in
    let region = cover $2 stop
    and value  = {kwd_function = $2;
                  param        = $3;
                  ret_type     = $4;
                  kwd_is       = $5;
                  return       = $6;
                  attributes   = $1}
    in {region; value} }

block:
  "begin" sep_or_term_list(statement,";") "end" {
     let statements, terminator = $2 in
     let region = cover $1 $3
     and value  = {enclosing = BeginEnd ($1,$3);
                   statements;
                   terminator}
     in {region; value}
  }
| "block" "{" sep_or_term_list(statement,";") "}" {
     let statements, terminator = $3 in
     let region = cover $1 $4
     and value  = {enclosing = Block ($1,$2,$4);
                   statements;
                   terminator}
     in {region; value} }

statement:
  instruction     { Instr $1 }
| open_data_decl  { Data  $1 }

open_data_decl:
  open_const_decl   { LocalConst       $1 }
| open_var_decl     { LocalVar         $1 }
| open_fun_decl     { LocalFun         $1 }
| open_type_decl    { LocalType        $1 }
| open_module_decl  { LocalModule      $1 }
| open_module_alias { LocalModuleAlias $1 }


open_var_decl:
  attributes "var" unqualified_decl(":=") {
    let pattern, var_type, assign, init, stop = $3 in
    let region = cover $2 stop
    and value  = {attributes=$1;
                  kwd_var=$2;
                  pattern;
                  var_type;
                  assign;
                  init;
                  terminator=None}
    in {region; value} }

unqualified_decl(OP):
  core_pattern ioption(type_annotation) OP expr {
    let region = expr_to_region $4
    in $1, $2, $3, $4, region }

const_decl:
  open_const_decl ";"? {
    {$1 with value = {$1.value with terminator=$2}} }

instruction:
  conditional  {        Cond $1 }
| case_instr   {   CaseInstr $1 }
| assignment   {      Assign $1 }
| loop         {        Loop $1 }
| call_instr    {    ProcCall $1 }
| "skip"       {        Skip $1 }
| record_patch { RecordPatch $1 }
| map_patch    {    MapPatch $1 }
| set_patch    {    SetPatch $1 }
| map_remove   {   MapRemove $1 }
| set_remove   {   SetRemove $1 }

set_remove:
  "remove" expr "from" "set" path {
    let region = cover $1 (path_to_region $5) in
    let value  = {kwd_remove = $1;
                  element    = $2;
                  kwd_from   = $3;
                  kwd_set    = $4;
                  set        = $5}
    in {region; value} }

map_remove:
  "remove" expr "from" "map" path {
    let region = cover $1 (path_to_region $5) in
    let value  = {kwd_remove = $1;
                  key        = $2;
                  kwd_from   = $3;
                  kwd_map    = $4;
                  map        = $5}
    in {region; value} }

set_patch:
  "patch" path "with" ne_injection("set",expr) {
    let set_inj = $4 (fun region -> NEInjSet region) in
    let region = cover $1 set_inj.region in
    let value  = {kwd_patch = $1;
                  path      = $2;
                  kwd_with  = $3;
                  set_inj}
    in {region; value} }

map_patch:
  "patch" path "with" ne_injection("map",binding) {
    let map_inj = $4 (fun region -> NEInjMap region) in
    let region  = cover $1 map_inj.region in
    let value   = {kwd_patch = $1;
                   path      = $2;
                   kwd_with  = $3;
                   map_inj}
    in {region; value} }

injection(Kind,element):
  Kind sep_or_term_list(element,";") "end" {
    fun mk_kwd ->
      let elements, terminator = $2 in
      let region = cover $1 $3
      and value  = {
        kind      = mk_kwd $1;
        enclosing = End $3;
        elements  = Some elements;
        terminator}
      in {region; value}
  }
| Kind "end" {
    fun mk_kwd ->
      let region = cover $1 $2
      and value  = {kind       = mk_kwd $1;
                    enclosing  = End $2;
                    elements   = None;
                    terminator = None}
      in {region; value}
  }
| Kind "[" sep_or_term_list(element,";") "]" {
    fun mk_kwd ->
      let elements, terminator = $3 in
      let region = cover $1 $4
      and value  = {kind      = mk_kwd $1;
                    enclosing = Brackets ($2,$4);
                    elements  = Some elements;
                    terminator}
      in {region; value}
  }
| Kind "[" "]" {
    fun mk_kwd ->
      let region = cover $1 $3
      and value  = {kind       = mk_kwd $1;
                    enclosing  = Brackets ($2,$3);
                    elements   = None;
                    terminator = None}
      in {region; value} }

ne_injection(Kind,element):
  Kind sep_or_term_list(element,";") "end" {
    fun mk_kwd ->
      let ne_elements, terminator = $2 in
      let region = cover $1 $3
      and value  = {kind      = mk_kwd $1;
                    enclosing = End $3;
                    ne_elements;
                    terminator;
                    attributes = []}
      in {region; value}
  }
| Kind "[" sep_or_term_list(element,";") "]" {
    fun mk_kwd ->
      let ne_elements, terminator = $3 in
      let region = cover $1 $4
      and value = {kind      = mk_kwd $1;
                   enclosing = Brackets ($2,$4);
                   ne_elements;
                   terminator;
                   attributes = []}
      in {region; value} }

binding:
  expr "->" expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {source = $1;
                  arrow  = $2;
                  image  = $3}
    in {region; value} }

record_patch:
  "patch" path "with" record_expr {
    let region = cover $1 $4.region in
    let value  = {kwd_patch  = $1;
                  path       = $2;
                  kwd_with   = $3;
                  record_inj = $4}
    in {region; value} }

call_instr:
  call_expr { $1 }

(* Conditionals instructions *)

conditional:
  "if" expr "then" if_clause ";"? "else" if_clause {
    let region = cover $1 (if_clause_to_region $7) in
    let value : CST.conditional = {
      kwd_if     = $1;
      test       = $2;
      kwd_then   = $3;
      ifso       = $4;
      terminator = $5;
      kwd_else   = $6;
      ifnot      = $7}
    in {region; value} }

if_clause:
  instruction  { ClauseInstr $1 }
| clause_block { ClauseBlock $1 }

clause_block:
  block { LongBlock $1 }
| "{" sep_or_term_list(statement,";") "}" {
    let region = cover $1 $3 in
    let value  = {lbrace=$1; inside=$2; rbrace=$3}
    in ShortBlock {value; region} }

(* Case instructions and expressions *)

case_instr:
  case(if_clause) { $1 if_clause_to_region }

case(rhs):
  "case" expr "of" "|"? cases(rhs) "end" {
    fun rhs_to_region ->
      let region = cover $1 $6 in
      let value  = {kwd_case  = $1;
                    expr      = $2;
                    kwd_of    = $3;
                    enclosing = End $6;
                    lead_vbar = $4;
                    cases     = $5 rhs_to_region}
      in {region; value}
  }
| "case" expr "of" "[" "|"? cases(rhs) "]" {
    fun rhs_to_region ->
      let region = cover $1 $7 in
      let value  = {kwd_case  = $1;
                    expr      = $2;
                    kwd_of    = $3;
                    enclosing = Brackets ($4,$7);
                    lead_vbar = $5;
                    cases     = $6 rhs_to_region}
      in {region; value} }

cases(rhs):
  nsepseq(case_clause(rhs),"|") {
    fun rhs_to_region ->
      let mk_clause pre_clause = pre_clause rhs_to_region in
      let value  = Utils.nsepseq_map mk_clause $1 in
      let region = nsepseq_to_region (fun x -> x.region) value
      in {region; value} }

case_clause(rhs):
  pattern "->" rhs {
    fun rhs_to_region ->
      let start  = pattern_to_region $1 in
      let region = cover start (rhs_to_region $3)
      and value  = {pattern=$1; arrow=$2; rhs=$3}
      in {region; value} }

assignment:
  lhs ":=" rhs {
    let stop   = expr_to_region $3 in
    let region = cover (lhs_to_region $1) stop
    and value  = {lhs = $1; assign = $2; rhs = $3}
    in {region; value} }

rhs:
  expr { $1 }

lhs:
  path       {    Path $1 }
| map_lookup { MapPath $1 }

(* Loops *)

loop:
  while_loop { $1 }
| for_loop   { $1 }

while_loop:
  "while" expr block {
    let region = cover $1 $3.region
    and value  = {kwd_while=$1; cond=$2; block=$3}
    in While {region; value} }

for_loop:
  "for" variable "->" variable "in" "map" expr block {
    let region = cover $1 $8.region in
    let value  = {kwd_for    = $1;
                  var        = $2;
                  bind_to    = Some ($3,$4);
                  kwd_in     = $5;
                  collection = Map $6;
                  expr       = $7;
                  block      = $8}
    in For (ForCollect {region; value})
  }
| "for" variable ":=" expr "to" expr ioption(step_clause) block {
    let region = cover $1 $8.region in
    let value  = {kwd_for = $1;
                  binder  = $2;
                  assign  = $3;
                  init    = $4;
                  kwd_to  = $5;
                  bound   = $6;
                  step    = $7;
                  block   = $8}
    in For (ForInt {region; value})
  }
| "for" variable "in" collection expr block {
    let region = cover $1 $6.region in
    let value  = {kwd_for    = $1;
                  var        = $2;
                  bind_to    = None;
                  kwd_in     = $3;
                  collection = $4;
                  expr       = $5;
                  block      = $6}
    in For (ForCollect {region; value}) }

step_clause:
  "step" expr { $1,$2 }

collection:
  "set"  { Set  $1 }
| "list" { List $1 }

(* Expressions *)

interactive_expr:
  expr EOF { $1 }

expr:
  case(expr) { ECase ($1 expr_to_region) }
| fun_expr   { EFun $1                   }
| block_with { EBlock $1                 }
| cond_expr
| disj_expr  { $1 }

block_with:
  block "with" expr {
    let start  = $2
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    let value  = {block=$1; kwd_with=$2; expr=$3}
    in {region; value} }

cond_expr:
  "if" expr "then" expr ";"? "else" expr {
    let region = cover $1 (expr_to_region $7) in
    let value : CST.cond_expr = {
      kwd_if     = $1;
      test       = $2;
      kwd_then   = $3;
      ifso       = $4;
      terminator = $5;
      kwd_else   = $6;
      ifnot      = $7}
    in ECond {region; value} }

disj_expr:
  conj_expr { $1 }
| disj_expr "or" conj_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3} in
    ELogic (BoolExpr (Or {region; value})) }

conj_expr:
  set_membership { $1 }
| conj_expr "and" set_membership {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in ELogic (BoolExpr (And {region; value})) }

set_membership:
  comp_expr { $1 }
| core_expr "contains" set_membership {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop in
    let value  = {set=$1; kwd_contains=$2; element=$3}
    in ESet (SetMem {region; value}) }

comp_expr:
  comp_expr "<"   cat_expr { mk_comp (fun reg -> Lt reg)    $1 $2 $3 }
| comp_expr "<="  cat_expr { mk_comp (fun reg -> Leq reg)   $1 $2 $3 }
| comp_expr ">"   cat_expr { mk_comp (fun reg -> Gt reg)    $1 $2 $3 }
| comp_expr ">="  cat_expr { mk_comp (fun reg -> Geq reg)   $1 $2 $3 }
| comp_expr "="   cat_expr { mk_comp (fun reg -> Equal reg) $1 $2 $3 }
| comp_expr "=/=" cat_expr { mk_comp (fun reg -> Neq reg)   $1 $2 $3 }
| cat_expr                 { $1 }

cat_expr:
  cons_expr "^" cat_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in EString (Cat {region; value})
  }
| cons_expr { $1 }

cons_expr:
  add_expr "#" cons_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {arg1=$1; op=$2; arg2=$3}
    in EList (ECons {region; value})
  }
| add_expr { $1 }

add_expr:
  add_expr "+" mult_expr { mk_arith (fun reg -> Add reg) $1 $2 $3 }
| add_expr "-" mult_expr { mk_arith (fun reg -> Sub reg) $1 $2 $3 }
| mult_expr              { $1 }

mult_expr:
  mult_expr "*"   unary_expr { mk_arith (fun reg -> Mult reg) $1 $2 $3 }
| mult_expr "/"   unary_expr { mk_arith (fun reg -> Div reg)  $1 $2 $3 }
| mult_expr "mod" unary_expr { mk_arith (fun reg -> Mod reg)  $1 $2 $3 }
| unary_expr                 { $1 }

unary_expr:
  "-" core_expr {
    let region = cover $1 (expr_to_region $2)
    and value  = {op=$1; arg=$2}
    in EArith (Neg {region; value})
  }
| "not" core_expr {
    let region = cover $1 (expr_to_region $2)
    and value  = {op=$1; arg=$2} in
    ELogic (BoolExpr (Not {region; value}))
  }
| core_expr { $1 }

core_expr:
  "<int>"                       { EArith (Int $1)              }
| "<nat>"                       { EArith (Nat $1)              }
| "<mutez>"                     { EArith (Mutez $1)            }
| "<ident>"                     { EVar $1                      }
| "<string>"                    { EString (String $1)          }
| "<verbatim>"                  { EString (Verbatim $1)        }
| "<bytes>"                     { EBytes $1                    }
| par(annot_expr)               { EAnnot $1                    }
| tuple_expr                    { ETuple $1                    }
| list_expr                     { EList $1                     }
| call_expr                     { ECall $1                     }
| value_in_module               { EModPath $1                  }
| map_expr                      { EMap $1                      }
| set_expr                      { ESet $1                      }
| record_expr                   { ERecord $1                   }
| record_update                 { EUpdate $1                   }
| code_inj                      { ECodeInj $1                  }
| "<uident>"                    { EConstr {$1 with value=$1,None} }
| "<uident>" arguments {
    let region = cover $1.region $2.region in
    EConstr {region; value = $1, Some $2} }

selected_expr:
  "or"       { EVar  {value="or"; region=$1} }
| field_name { EVar  $1 }
| projection { EProj $1 }

(* Function calls *)

call_expr:
  path_expr arguments {
    let start  = expr_to_region $1 in
    let region = cover start $2.region
    in mk_reg region ($1,$2) }

(* Path expressions

   A path expression is a construct that qualifies unambiguously a
   value or type. When maintaining this subgrammar, be wary of not
   introducing a regression. Here are the currently possible cases:

      * a single variable: "a"
      * a single variable in a nested module: "A.B.a"
      * nested fields and compoments from a variable: "a.0.1.b"
      * same withing a nested module: "A.B.a.0.1.b"
      * nested fields and components from an expression: "(e).a.0.1.b"
 *)

path_expr:
  value_in_module | local_path { $1 }

value_in_module:
  module_name "." "or"    { mk_EVar $1 "or"      }
| module_name "." "and"   { mk_EVar $1 "and"     }
| module_path(field_path) {
    EModPath (mk_mod_path $1 CST.expr_to_region) }

field_path:
  variable "." nsepseq(selection,".") {
    let start  = $1.region
    and stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover start stop
    and value  = {record=$1; selector=$2; field_path=$3}
    in EProj {region; value}
  }
| variable { EVar $1 }

annot_expr:
  disj_expr ":" type_expr { $1,$2,$3 }

set_expr:
  injection("set",expr) { SetInj ($1 (fun region -> InjSet region)) }

map_expr:
  map_lookup {
    MapLookUp $1
  }
| injection("map",binding) {
    MapInj ($1 (fun region -> InjMap region))
  }
| injection("big_map",binding) {
    BigMapInj ($1 (fun region -> InjBigMap region)) }

path:
  variable   { Name $1 }
| projection { Path $1 }

projection:
  struct_name "." nsepseq(selection,".") {
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover $1.region stop
    and value  = {struct_name=$1; selector=$2; field_path=$3}
    in {region; value} }

selection:
  field_name { FieldName $1 }
| "<int>"    { Component $1 }

local_path:
  par(expr) "." nsepseq(selection,".") {
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover $1.region stop
    and value  = {record=$1; selector=$2; field_path=$3}
    in E_Proj {region; value}
  }
| field_path { $1 }

record_expr:
  ne_injection("record",field_assignment) {
    $1 (fun region -> NEInjRecord region) }

field_assignment:
  field_name "=" expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = {field_name=$1; assignment=$2; field_expr=$3}
    in {region; value} }

record_update:
  path "with" ne_injection("record",field_path_assignment) {
    let updates = $3 (fun region -> NEInjRecord region) in
    let region  = cover (path_to_region $1) updates.region in
    let value   = {record=$1; kwd_with=$2; updates}
    in {region; value} }

field_path_assignment:
  path "=" expr {
    let region = cover (path_to_region $1) (expr_to_region $3)
    and value  = {field_path=$1; assignment=$2; field_expr=$3}
    in {region; value} }

code_inj:
  "[%lang" expr "]" {
    let region   = cover $1.region $3
    and value    = {language=$1; code=$2; rbracket=$3}
    in {region; value} }

tuple_expr:
  par(tuple_comp) { $1 }

tuple_comp:
  expr "," nsepseq(expr,",") { Utils.nsepseq_cons $1 $2 $3 }

arguments:
  par(nsepseq(expr,",")) { $1 }

list_expr:
  injection("list",expr) { EListComp ($1 (fun region -> InjList region)) }
| "nil"                  { ENil $1                                     }

(* Patterns *)

pattern:
  core_pattern { $1 }
| core_pattern "#" nsepseq(core_pattern,"#") {
    let value = Utils.nsepseq_cons $1 $2 $3 in
    let region = nsepseq_to_region pattern_to_region value
    in PList (PCons {region; value}) }

core_pattern:
  "<int>"        { PInt    $1 }
| "<nat>"        { PNat    $1 }
| "<bytes>"      { PBytes  $1 }
| "<string>"     { PString $1 }
| "_"            { PVar    (mk_wild $1) }
| var_pattern    { PVar    $1 }
| list_pattern   { PList   $1 }
| tuple_pattern  { PTuple  $1 }
| ctor_pattern { PConstr $1 }
| record_pattern { PRecord $1 }

var_pattern:
  attributes "<ident>" {
    let value = {variable=$2; attributes=$1}
    in {$2 with value} }

field_pattern:
  field_name {
    let region = $1.region in
    let pattern = PVar {region; value = {variable=$1; attributes=[]}} in
    let value   = {field_name=$1; eq=Region.ghost; pattern}
    in {region; value}
  }
| "_" { failwith "TODO" }
| field_name "=" core_pattern {
    let start  = $1.region
    and stop   = pattern_to_region $3 in
    let region = cover start stop
    and value  = {field_name=$1; eq=$2; pattern=$3}
    in {region; value} }

record_pattern:
  injection("record", field_pattern) {
    $1 (fun region -> InjRecord region) }

list_pattern:
  "nil"                          {      PNil $1 }
| par(cons_pattern)              {  PParCons $1 }
| injection("list",core_pattern) {
    PListComp ($1 (fun region -> InjList region)) }

cons_pattern:
  core_pattern "#" pattern { $1,$2,$3 }

tuple_pattern:
  par(nsepseq(core_pattern,",")) { $1 }

ctor_pattern:
  "<uident>" ioption(tuple_pattern) {
    let region = match $2 with
                   None -> $1.region
                 | Some stop -> cover $1.region stop.region
    in {region; value = ($1,$2)} }
