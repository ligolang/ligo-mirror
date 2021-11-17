(* Menhir specification of the parser of PascaLIGO

   "Beaucoup de mal durable est souvent fait par les choses provisoires."
                                              Victor Hugo, 11 Sept. 1848
                         http://classes.bnf.fr/laicite/anthologie/32.htm

   About Menhir:
     http://gallium.inria.fr/blog/parser-construction-menhir-appetizers/
     http://gallium.inria.fr/~fpottier/menhir/manual.pdf

   This grammar specification actually encompasses two dialects of
   PascaLIGO. One is called _verbose_ because it features more
   keywords, and the other one is called _terse_. Generally speaking,
   the latter replaces many keywords 'end' by using symbols
   instead. An example of verbose record type definition would be:

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
   remove a reduction on error (%on_error_reduce) that is related to
   your change.

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

     We do not rely on predefined Menhir symbols, like
   [$symbolstartpos], to help determine the regions (that is, source
   locations) of our tokens and subtrees of our CST. One reason is
   that the semantic of [$symbolstartpos] is that of ocamlyacc, and
   this does not blend well with nullable prefixes of rules. That is
   why we use [Region.cover] to compute the region in the source that
   corresponds to any given CST node. This is more verbose than
   letting Menhir ask the lexer buffer with an awkward semantic, but
   we are 100% in control.

     A note on terminology: I sometimes use words taken from the
   context of formal logic or programming theory. For example:
   https://en.wikipedia.org/wiki/Extensional_and_intensional_definitions

     We always define and write values of type [Region.t] by stating
   the region first, like so: [{region; value}], and we always use
   name punning. *)

%{
(* START HEADER *)

[@@@warning "-42"]

(* Vendors dependencies *)

open Simple_utils.Region

(* LIGO dependencies *)

module CST = Cst_pascaligo.CST
open! CST
module Wrap = Lexing_shared.Wrap

(* UTILITIES

   The following functions help build CST nodes. When they are
   complicated, like [mk_mod_path], it is because the grammar rule had
   to be written in a certain way to remain LR, and that way did not
   make it easy in the semantic action to collate the information into
   CST nodes. *)

let unwrap = Wrap.payload
let wrap   = Wrap.wrap

let mk_reg region value = Region.{region; value}

let apply_type_ctor token args =
  let {region; value = {lpar; inside; rpar}} = args in
  let tuple  = mk_reg region {lpar; inside=inside,[]; rpar}
  and region = cover token#region args.region
  in mk_reg region (T_Var token, tuple)

let apply_map token args =
  let region = cover token#region args.region
  in mk_reg region (T_Var token, args)

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
    let region = CST.nseq_to_region (fun (x,_) -> x#region) nseq in
    let region = Region.cover region (to_region field)
    and value = {module_path; selector=last_dot; field}
    in {value; region}

(* The function [terminate_decl] adds a semicolon terminator to a
   declaration. It is a functional update because we want to share
   rules for declarations that are valid at top-level and at inner
   levels (blocks), and the latter require a semicolon --- which we
   patch afterwards in semantic actions. *)

let terminate_decl semi = function
  D_Const     d -> D_Const     {d with value = {d.value with terminator=semi}}
| D_Directive d -> D_Directive d
| D_Fun       d -> D_Fun       {d with value = {d.value with terminator=semi}}
| D_Module    d -> D_Module    {d with value = {d.value with terminator=semi}}
| D_ModAlias  d -> D_ModAlias  {d with value = {d.value with terminator=semi}}
| D_Type      d -> D_Type      {d with value = {d.value with terminator=semi}}

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

%on_error_reduce nsepseq(core_type,TIMES)
%on_error_reduce ctor_app_pattern
%on_error_reduce nseq(__anonymous_5)
%on_error_reduce var_pattern
%on_error_reduce module_path(__anonymous_3)
%on_error_reduce field_decl
%on_error_reduce test_clause(closed_instr)
%on_error_reduce base_expr(closed_expr)
%on_error_reduce base_expr(expr)
%on_error_reduce base_instr(closed_instr,closed_expr)
%on_error_reduce bin_op(disj_expr_level,Or,conj_expr_level)
%on_error_reduce bin_op(conj_expr_level,And,set_mem_level)
%on_error_reduce bin_op(add_expr_level,PLUS,mult_expr_level)
%on_error_reduce bin_op(add_expr_level,MINUS,mult_expr_level)
%on_error_reduce nseq(__anonymous_0(field_pattern,SEMI))
%on_error_reduce nsepseq(field_pattern,SEMI)
%on_error_reduce field_pattern
%on_error_reduce core_expr
%on_error_reduce nsepseq(module_name,DOT)
%on_error_reduce ctor_app_expr
%on_error_reduce nseq(__anonymous_0(field_decl,SEMI))
%on_error_reduce nseq(__anonymous_0(field_path_assignment,SEMI))
%on_error_reduce nseq(__anonymous_0(binding,SEMI))
%on_error_reduce nseq(__anonymous_0(core_pattern,SEMI))
%on_error_reduce nseq(__anonymous_0(expr,SEMI))
%on_error_reduce nseq(__anonymous_0(statement,SEMI))
%on_error_reduce nsepseq(case_clause(expr),VBAR)
%on_error_reduce nsepseq(core_pattern,SEMI)
%on_error_reduce pattern
%on_error_reduce nsepseq(case_clause(test_clause(instruction)),VBAR)
%on_error_reduce lhs
%on_error_reduce nsepseq(statement,SEMI)
%on_error_reduce update_expr_level
%on_error_reduce nsepseq(param_decl,SEMI)
%on_error_reduce nsepseq(selection,DOT)
%on_error_reduce field_path
%on_error_reduce nsepseq(binding,SEMI)
%on_error_reduce nsepseq(expr,SEMI)
%on_error_reduce add_expr_level
%on_error_reduce unary_expr_level
%on_error_reduce const_decl
%on_error_reduce fun_decl
%on_error_reduce variant
%on_error_reduce core_type
%on_error_reduce nsepseq(field_decl,SEMI)
%on_error_reduce type_decl
%on_error_reduce cartesian_level
%on_error_reduce fun_type_level
%on_error_reduce cons_expr_level
%on_error_reduce cat_expr_level
%on_error_reduce set_mem_level
%on_error_reduce disj_expr_level
%on_error_reduce nsepseq(variant,VBAR)
%on_error_reduce core_pattern
%on_error_reduce expr
%on_error_reduce option(SEMI)
%on_error_reduce nseq(top_declaration)
%on_error_reduce nseq(Attr)
%on_error_reduce qualified_type

%%

(* RULES *)

(* Compound constructs *)

par(X):
  "(" X ")" {
    let region = cover $1#region $3#region
    and value  = {lpar=$1; inside=$2; rpar=$3}
    in {region; value} }

brackets(X):
  "[" X "]" {
    let region = cover $1#region $3#region
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

(* Aliasing and inlining some tokens *)

%inline variable        : "<ident>"  { $1 }
%inline type_param      : "<ident>"  { $1 }
%inline type_name       : "<ident>"  { $1 }
%inline type_ctor       : "<ident>"  { T_Var $1 }
%inline fun_name        : "<ident>"  { $1 }
%inline field_name      : "<ident>"  { $1 }
%inline record_or_tuple : "<ident>"  { $1 }
%inline module_name     : "<uident>" { $1 }
%inline ctor            : "<uident>" { $1 }

(* Unary operators *)

unary_op(op,arg):
  op arg {
    let region = cover $1#region (expr_to_region $2)
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

%inline
attributes:
  ioption(nseq("[@attr]") { Utils.nseq_to_list $1 }) {
    match $1 with None -> [] | Some list -> list }

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

(* Type declarations *)

type_decl:
  "type" type_name ioption(type_params) "is" type_expr {
    let stop   = type_expr_to_region $5 in
    let region = cover $1#region stop in
    let value  = {kwd_type=$1; name=$2; params=$3; kwd_is=$4;
                  type_expr=$5; terminator=None}
    in {region; value} }

type_params:
  par(nsepseq(type_param,",")) { $1 }

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
   like so: [t -> [`Foo of string * int]]. *)

type_expr:
  fun_type_level | sum_type { $1 }

fun_type_level:
  cartesian_level "->" fun_type_level {
    let start  = type_expr_to_region $1
    and stop   = type_expr_to_region $3 in
    let region = cover start stop in
    T_Fun {region; value = $1,$2,$3}
  }
| cartesian_level { $1 }

cartesian_level:
  core_type "*" nsepseq(core_type,"*") {
    let start  = type_expr_to_region $1
    and stop   = nsepseq_to_regiontype_expr_to_region $3 in
    let region = cover start stop in
    T_Cart {region; value = $1,$2,$3}
  }
| core_type { $1 }

core_type:
  "<string>"     { T_String (unwrap $1)  }
| "<int>"        { T_Int    (unwrap $1)  }
| "_"            { T_Var    $1           }
| type_ctor_app  { T_App    $1           }
| record_type    { T_Record $1           }
| type_name      { T_Var    $1           }
| par(type_expr) { T_Par    $1           }
| qualified_type { $1 }

(* Type constructor applications *)

type_ctor_app:
  type_ctor type_tuple {
    let region = cover (type_expr_to_region $1) $2.region
    in mk_reg region ($1,$2)
  }
| "map"     type_tuple
| "big_map" type_tuple     { apply_map       $1 $2 }
| "set"     par(type_expr)
| "list"    par(type_expr) { apply_type_ctor $1 $2 }

type_tuple: par(nsepseq(type_expr,",")) { $1 }

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

qualified_type:
  type_in_module(type_ctor) type_tuple {
    let region = cover (type_expr_to_region $1) $2.region
    in T_App {region; value=$1,$2}
  }
| type_in_module(type_name      { T_Var $1 })
| type_in_module(par(type_expr) { T_Par $1 }) { $1 }

type_in_module(type_expr):
  module_path(type_expr) {
    T_ModPath (mk_mod_path $1 type_expr_to_region) }

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

sum_type:
  nsepseq(variant,"|") {
    let region = nsepseq_to_region (fun x -> x.region) $1 in
    let value  = {variants=$1; attributes=[]; lead_vbar=None}
    in T_Sum {region; value}
  }
| attributes "|" nsepseq(variant,"|") {
    let region = nsepseq_to_region (fun x -> x.region) $3 in
    let value  = {variants=$3; attributes=$1; lead_vbar = Some $2}
    in T_Sum {region; value} }

(* Reminder: Always use [ioption] at the end of a rule *)

variant:
  attributes ctor ioption(of_type_expr) {
    let stop   = match $3 with
                   None -> $2#region
                 | Some (_, t) -> type_expr_to_region t in
    let region = cover $2#region stop
    and value  = {ctor=$2; args=$3; attributes=$1}
    in {region; value} }

of_type_expr: "of" fun_type_level { $1,$2 }

(* Record types *)

record_type:
  compound("record",field_decl) { $1 }

(* When the type annotation is missing in a field declaration, the
   type of the field is the name of the field. *)

field_decl:
  attributes field_name ioption(type_annotation) {
    let stop = match $3 with
                        None -> $2#region
               | Some (_, t) -> type_expr_to_region t in
    let region = match $1 with
                         [] -> cover $2#region stop
                 | start::_ -> cover start.region stop
    and value = {attributes=$1; field_name=$2; field_type=$3}
    in {region; value} }

(* Inlining the definition of the non-terminal [type_annotation]
   enables the syntactic context of its occurrences, yielding more
   precise syntax error messages. (Otherwise we have only one error
   message about a type annotation in all contexts.) *)

%inline
type_annotation: ":" type_expr { $1,$2 }

(* Constant declarations *)

(* Note the use of the rule [unqualified_decl]. It takes a parameter
   that derives the kind of definitional symbol. In the case of
   constants, that symbol is the mathematical equality '='. The case
   of "var" variables, which are not allowed in top-level
   declarations, that symbol would be ':='. See [var_decl] below. *)

const_decl:
  attributes "const" unqualified_decl("=") {
    let pattern, const_type, equal, init, stop = $3 in
    let region = cover $2#region stop
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
    let kwd_recursive = $2 in
    let kwd_function = $3 in
    let kwd_is = $7 in
    let start  = match kwd_recursive with
                   Some start -> start
                 | None -> kwd_function in
    let stop   = expr_to_region $8 in
    let region = cover start#region stop
    and value  = {attributes=$1; kwd_recursive=$2; kwd_function=$3;
                  fun_name=$4; param=$5; ret_type=$6; kwd_is=$7;
                  return=$8; terminator=None}
    in {region; value} }

parameters: par(nsepseq(param_decl,";")) { $1 }

param_decl:
  param_kind var_pattern ioption(param_type) {
    let kind_reg, param_kind = $1
    and stop   = match $3 with
                         None -> $2#region
                 | Some (_,t) -> type_expr_to_region t in
    let region = cover kind_reg stop
    and value  = {param_kind; var=$2; param_type=$3}
    in {region; value} }

param_kind:
  "var"   { $1#region, `Var   $1 }
| "const" { $1#region, `Const $1 }

var_pattern:
  attributes variable { $2#set_attributes $1 }
| "_"                 { $1                   }

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
    let region    = cover $1#region $7#region
    and value     = {kwd_module=$1; name=$2; kwd_is=$3; enclosing;
                     declarations=$6; terminator=None}
    in {region; value} }

verb_module_decl:
  "module" module_name "is" "begin" declarations "end" {
    let enclosing = BeginEnd ($4,$6) in
    let region    = cover $1#region $6#region
    and value     = {kwd_module=$1; name=$2; kwd_is=$3; enclosing;
                     declarations=$5; terminator=None}
    in {region; value} }

declarations:
  nseq(declaration ";"? { terminate_decl $2 $1}) { $1 }

(* Module aliases *)

module_alias:
  "module" module_name "is" nsepseq(module_name,".") {
    let stop   = nsepseq_to_region (fun x -> x#region) $4 in
    let region = cover $1#region stop in
    let value  = {kwd_module=$1; alias=$2; kwd_is=$3;
                  mod_path=$4; terminator=None}
    in {region; value} }

(* STATEMENTS *)

statement:
  declaration { S_Decl    $1 }
| var_decl    { S_VarDecl $1 } (* Not allowed at top-level *)
| instruction { S_Instr   $1 } (* Not allowed at top-level *)

(* Variable declarations *)

(* Programming theory jargon calls "variables" any name, which is
   unfortunate in the case that name denotes a constant, or _immutable
   data_. In PascaLIGO, we distinguish between constants and "variable
   variables", the former being qualified by the keyword "const" at
   their declaration, and the latter by "var". The rule [var_decl]
   describes those variables that are mutable. Notice that the
   definitional symbol is ':=' instead of '=' for constants. *)

var_decl:
  "var" unqualified_decl(":=") {
    let pattern, var_type, assign, init, stop = $2 in
    let region = cover $1#region stop
    and value  = {kwd_var=$1; pattern; var_type;
                  assign; init; terminator=None; attributes=[]}
    in {region; value} }

(* INSTRUCTIONS *)

(* The rule [base_instr] is parameterised by an expression
   [right_expr] because [assignment], [remove_instr] and [patch_instr]
   can derive [right_expr] to the right. This has an impact on the
   so-called "dangling else" problem because both [instruction] and
   [expr] have conditionals. For example

   if a then x := if b then c else d

   could either mean

   if a then (x := if b then c) else d

   or

   if a then x := (if b then c else d)

   The latter is our interpretation.

   Compare with [base_expr] below, which is a bit simpler, as
   expressions do not depend on instructions. *)

instruction:
  base_instr(instruction,expr)
| if_then_instr(instruction) { $1 }

base_instr(right_instr,right_expr):
  if_then_else_instr(right_instr) { I_Cond   $1 }
| remove_instr(right_expr)        { I_Remove $1 }
| patch_instr(right_expr)         { I_Patch  $1 }
| assignment(right_expr)          { I_Assign $1 }
| case_instr                      { I_Case   $1 }
| call_instr                      { I_Call   $1 }
| for_int                         { I_For    $1 }
| for_in                          { I_ForIn  $1 }
| while_loop                      { I_While  $1 }
| "skip"                          { I_Skip   $1 }

(* Conditional instructions (see [cond_expr] below for comparison) *)

if_then_instr(right_instr):
  "if" expr "then" test_clause(right_instr) {
     let region = cover $1#region (test_clause_to_region $4)
     and value  = {kwd_if=$1; test=$2; kwd_then=$3; if_so=$4;
                   if_not=None}
     in I_Cond {region; value} }

if_then_else_instr(right_instr):
  "if" expr "then" test_clause(closed_instr)
  "else" test_clause(right_instr) {
     let region = cover $1#region (test_clause_to_region $6)
     and value  = {kwd_if=$1; test=$2; kwd_then=$3; if_so=$4;
                   if_not = Some ($5,$6) }
     in {region; value} }

closed_instr:
  base_instr(closed_instr,closed_expr) { $1 }

(* Removing from sets and maps *)

remove_instr(right_expr):
  "remove" expr "from" right_expr {
    let region = cover $1#region (expr_to_region $4)
    and value  = {kwd_remove=$1; item=$2; kwd_from=$3; collection=$4}
    in {region; value} }

(* Patches to sets, maps and records *)

patch_instr(right_expr):
  "patch" core_expr "with" right_expr {
    let region = cover $1#region (expr_to_region $4)
    and value  = {kwd_patch=$1; collection=$2; kwd_with=$3; patch=$4}
    in {region; value} }

(* Procedure calls *)

call_instr: call_expr { $1 }

(* Generic case construct. A case construct features pattern
   matching and it can either be an instruction or an expression,
   depending on the syntactic category allowed on the right-hand sides
   (rhs) of each individual case clause. The first rule [case(rhs)] is
   the terse version, the second the verbose one. Since the rule is
   parameterised by the right-hand side, we need to return in the
   semantic action a function parameterised by a function projecting
   the region out of it -- see parameter [rhs_to_region]. *)

case(rhs):
  terse_case(rhs) | verb_case(rhs) { $1 }

terse_case(rhs):
  "case" expr "of" "[" ioption("|") cases(rhs) "]" {
    fun rhs_to_region ->
      let enclosing = Brackets ($4,$7)
      and cases     = $6 rhs_to_region in
      let region    = cover $1#region $7#region
      and value     = {kwd_case=$1; expr=$2; kwd_of=$3;
                       enclosing; lead_vbar=$5; cases}
      in {region; value} }

verb_case(rhs):
  "case" expr "of" ioption("|") cases(rhs) "end" {
    fun rhs_to_region ->
      let enclosing = End $6
      and cases     = $5 rhs_to_region in
      let region    = cover $1#region $6#region
      and value     = {kwd_case=$1; expr=$2; kwd_of=$3;
                       enclosing; lead_vbar=$4; cases}
      in {region; value} }

cases(rhs):
  nsepseq(case_clause(rhs),"|") {
    fun rhs_to_region ->
      let mk_clause pre_clause = pre_clause rhs_to_region
      in Utils.nsepseq_map mk_clause $1 }

case_clause(rhs):
  pattern "->" rhs {
    fun rhs_to_region ->
      let start  = pattern_to_region $1 in
      let region = cover start (rhs_to_region $3)
      and value  = {pattern=$1; arrow=$2; rhs=$3}
      in {region; value} }

(* Case instructions

   (See [case(expr)] below for comparison). The clauses' right-hand
   sides are [test_clause(instruction)]. Note how, following Pascal
   syntax, we allow a single instruction in the conditional branches,
   instead of always forcing the user to open a block, which would be
   verbose. *)

case_instr:
  case(test_clause(instruction)) { $1 test_clause_to_region }

test_clause(instr):
  instr { ClauseInstr $1 }
| block { ClauseBlock $1 }

block: terse_block | verb_block { $1 }

terse_block:
  "block"? "{" sep_or_term_list(statement,";") "}" {
     let statements, terminator = $3
     and enclosing : block_enclosing = Braces ($1,$2,$4) in
     let start  = match $1 with None -> $2 | Some b -> b in
     let region = cover start#region $4#region
     and value  = {enclosing; statements; terminator}
     in {region; value} }

verb_block:
  "begin" sep_or_term_list(statement,";") "end" {
     let statements, terminator = $2
     and enclosing : block_enclosing = BeginEnd ($1,$3) in
     let region    = cover $1#region $3#region
     and value     = {enclosing; statements; terminator}
     in {region; value} }

(* Assignments

   The left-hand side (rhs) of an assignment can only be a path, but
   the semantic action is a general expression. This means that the
   parser is more restrictive that what the CST will tell. *)

assignment(right_expr):
  lhs ":=" right_expr {
    let stop   = lhs_to_region $3 in
    let region = cover (expr_to_region $1) stop
    and value  = {lhs=$1; assign=$2; rhs=$3}
    in {region; value} }

lhs: path_expr | map_lookup { $1 }

(* Loops *)

while_loop:
  "while" expr block {
    let region = cover $1#region $3.region
    and value  = {kwd_while=$1; cond=$2; block=$3}
    in {region; value} }

for_int:
  "for" variable ":=" expr "to" expr ioption(step_clause) block {
    let region = cover $1#region $8.region in
    let value  = {kwd_for=$1; index=$2; assign=$3; init=$4;
                  kwd_to=$5; bound=$6; step=$7; block=$8}
    in {region; value} }

step_clause: "step" expr { $1,$2 }

for_in:
  "for" variable "->" variable "in" ioption("map") expr block {
    let bind_to  = Some ($3,$4) in
    let region   = cover $1#region (expr_to_region $7)
    and value    = {kwd_for=$1; var=$2; bind_to; kwd_in=$5;
                    expr=$7; block=$8}
    in {region; value}
  }
| "for" variable "in" expr block {
    let region = cover $1#region $5.region in
    let value  = {kwd_for=$1; var=$2; bind_to=None; kwd_in=$3;
                  expr=$4; block=$5}
    in {region; value} }

(* EXPRESSIONS *)

interactive_expr: expr EOF { $1 }

(* Our definition of [expr] aims at solving the "dangling else"
   problem in the grammar itself. That syntactical ambiguity arises
   when there is in a language a _closed_ conditional of the form "if
   e1 then e2 else e3" and an _open_ one of the form "if e1 then
   e2". Indeed,

     if a then if b then c else d

   could be construed either as

     if a then (if b then c) else d

   or

     if a then (if b then c else d)

   The latter is the standard interpretation ("The 'else' is hooked to
   the closest 'then' before.").

   To achieve this interpretation, two aims are achieved:

     * separating the conditional expressions ([cond_expr]) from the
       others ([base_expr]),

     * identifying the productions that are right-recursive.

     The first point is needed to distinguish if-then from
   if-then-else conditionals.

     The second point enables us to parameterise [base_expr] with
   [right_expr], for the benefit of those right-recursive
   rules. Depending on the right context, [base_expr] will be
   instanciated with different kinds of expressions. For example, we
   need [close_expr] to be an expression that is always suitable in
   the "then" branch of a _closed_ conditional, but a general
   expression is always suitable in the "then" branch of an open
   conditional. So the [right_expr] parameter is useful because, in
   the "then" branch of a closed conditional, we do not want to
   generate to its right an expression that is an open conditional,
   e.g.

      if a then fun x -> if x then b else c // dangling else!

   could be misconstrued as

      if a then (fun x -> if x then b) else c // wrong

   Instead, using [close_expr] in the "then" branch of a close
   conditional yields the intended goal as if we had written

      if a then (fun x -> if x then b else c)

   where "if x then b else c" has been derived by [expr].

   Note [disj_expr_level] in [base_expr]: this is the start of the
   stratification of the later in order to built in the grammar the
   priority of different operators/constructs in the usual handmade
   manner. So the sooner a non-terminal is defined, the lower its
   priority. For example, [disj_expr_level] is derived from [expr]
   before [conj_expr_level] because "or" has a lower priority than
   "and", as expected in Boolean algebras. *)

expr:
  base_expr(expr) | if_then_expr(expr) { $1 }

base_expr(right_expr):
  if_then_else_expr(right_expr) { E_Cond  $1 }
| block_with(right_expr)        { E_Block $1 }
| fun_expr(right_expr)          { E_Fun   $1 }
| case(expr)                    { E_Case ($1 expr_to_region) }
| disj_expr_level               { $1 }

(* Conditional expressions

   The CST node [E_Cond] is used in the semantic action of rule
   [if_then_expr], but not [if_then_else]. This enables a smoother,
   more uniform reading of the semantic actions above.

   Note how beautiful rule [closed_expr] is. We could have duplicated
   instead [base_expr], but we did not, thanks to the rule
   parameterisation of Menhir, which is a very powerful design
   feature at work here. *)

if_then_expr(right_expr):
  "if" expr "then" right_expr {
     let region = cover $1#region (expr_to_region $4)
     and value = {kwd_if=$1; test=$2; kwd_then=$3; if_so=$4; if_not=None }
     in E_Cond {region; value} }

if_then_else_expr(right_expr):
  "if" expr "then" closed_expr "else" right_expr {
     let region = cover $1#region (expr_to_region $6)
     and value = {kwd_if=$1; test=$2; kwd_then=$3; if_so=$4;
                  if_not = Some ($5,$6) }
     in {region; value} }

closed_expr: base_expr(closed_expr) { $1 }

(* Block expressions *)

block_with(right_expr):
  block "with" right_expr {
    let region = cover $1.region (expr_to_region $3)
    and value  = {block=$1; kwd_with=$2; expr=$3}
    in {region; value} }

(* Functional expressions (a.k.a. lambdas) *)

fun_expr(right_expr):
  attributes "function" parameters ioption(type_annotation) "is" right_expr {
    let region = cover $2#region (expr_to_region $6)
    and value  = {kwd_function=$2; param=$3; ret_type=$4;
                  kwd_is=$5; return=$6; attributes=$1}
    in {region; value} }

(* Resuming stratification of [base_expr] with Boolean expressions *)

disj_expr_level:
  bin_op(disj_expr_level,"or",conj_expr_level) { E_Or $1 }
| conj_expr_level                              { $1 }

conj_expr_level:
  bin_op(conj_expr_level,"and",set_mem_level) { E_And $1 }
| set_mem_level                               { $1 }

(* Set membership *)

set_mem_level:
  update_expr_level "contains" set_mem_level {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {set=$1; kwd_contains=$2; element=$3}
    in E_SetMem {region; value}
  }
| comp_expr_level { $1 }

(* Comparisons

   Note that we made the choice of using the parameterised rule
   [bin_op] instead of a more direct definition, like

   comp_expr_level:
     comp_expr_level "<" cat_expr_level { ... }
   | ...

   This is because we wanted the simplest possible semantic actions:
   making a CST node, to avoid mistakes and confuse two operators. *)

comp_expr_level:
  bin_op(comp_expr_level, "<",   cat_expr_level) { E_Lt    $1 }
| bin_op(comp_expr_level, "<=",  cat_expr_level) { E_Leq   $1 }
| bin_op(comp_expr_level, ">",   cat_expr_level) { E_Gt    $1 }
| bin_op(comp_expr_level, ">=",  cat_expr_level) { E_Geq   $1 }
| bin_op(comp_expr_level, "=",   cat_expr_level) { E_Equal $1 }
| bin_op(comp_expr_level, "=/=", cat_expr_level) { E_Neq   $1 }
| cat_expr_level                                 { $1 }

(* Concatenation *)

cat_expr_level:
  bin_op(cons_expr_level, "^", cat_expr_level) { E_Cat $1 }
| cons_expr_level                              { $1 }

(* Consing *)

cons_expr_level:
  bin_op(add_expr_level, "#", cons_expr_level) { E_Cons $1 }
| add_expr_level                               { $1 }

(* Arithmetic expressions *)

add_expr_level:
  bin_op(add_expr_level, "+", mult_expr_level) { E_Add $1 }
| bin_op(add_expr_level, "-", mult_expr_level) { E_Sub $1 }
| mult_expr_level                              { $1 }

mult_expr_level:
  bin_op(mult_expr_level, "*",   unary_expr_level) { E_Mult $1 }
| bin_op(mult_expr_level, "/",   unary_expr_level) { E_Div  $1 }
| bin_op(mult_expr_level, "mod", unary_expr_level) { E_Mod  $1 }
| unary_expr_level                                 { $1 }

(* Unary expressions *)

unary_expr_level:
  unary_op("-",   update_expr_level) { E_Neg $1 }
| unary_op("not", update_expr_level) { E_Not $1 }
| update_expr_level                  { $1 }

(* The rationale for the existence of [update_expr_level] is that we
   use the keyword "with" in two different contexts: record updates
   and patches (see [instruction]) and the latter can derive the
   former, leading to a conflict, like so:

     patch (e) with ...

   could be the prefix of (adding parentheses):

     patch ((e) with ...) with ...

   Here, we enforce the first interpretation because of the rules for
   patches starting with:

    "patch" core_expr "with"
 *)

update_expr_level:
  update    { E_Update $1 }
| core_expr { $1 }

core_expr:
  "<int>"         { E_Int       $1 }
| "<nat>"         { E_Nat       $1 }
| "<mutez>"       { E_Mutez     $1 }
| "<string>"      { E_String    $1 }
| "<verbatim>"    { E_Verbatim  $1 }
| "<bytes>"       { E_Bytes     $1 }
| "nil"           { E_Nil       $1 }
| tuple_expr      { E_Tuple     $1 }
| list_expr       { E_List      $1 }
| record_expr     { E_Record    $1 }
| code_inj        { E_CodeInj   $1 }
| ctor_app_expr   { E_App       $1 }
| map_expr        { E_Map       $1 }
| big_map_expr    { E_BigMap    $1 }
| set_expr        { E_Set       $1 }
| par(typed_expr) { E_Typed     $1 }
| call_expr       { E_Call      $1 }
| map_lookup
| path_expr       { $1 }

(* Map expressions (extensional definitions) *)

map_expr:
  compound("map",binding) { $1 }

big_map_expr:
  compound("big_map",binding) { $1 }

binding:
  expr "->" expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {source=$1; arrow=$2; image=$3}
    in {region; value} }

(* Set expressions (extensional definitions) *)

set_expr:
  compound("set",expr) { $1 }

(* List expressions (extensional definitions) *)

list_expr:
  compound("list",expr) { $1 }

(* Record expressions

   The extensional definitions of records can make use of _punning_,
   by which the right-hand side of a field assigment is taken to be
   the variable is the current scope which is the same has the field
   name. This is a feature inspired by OCaml:

  https://ocaml.org/releases/4.12/htmlman/coreexamples.html#s%3Atut-recvariants

   Note that [field_path_assignment] starts with a [field_path]
   instead of a [field_name]. This is because we wanted the production
   [patch] to allow an expression after [with]. It is up to the tree
   abstraction pass to filter out those field path assigmnents that
   are not field paths in the context of a record expression _not_
   used as a patch. *)

record_expr:
  compound("record",field_path_assignment) { $1 }

field_path_assignment:
  attributes field_path "=" expr {
    let region = cover (expr_to_region $2) (expr_to_region $4)
    and value  = Complete {field_lhs=$2; assign=$3;
                           field_rhs=$4; attributes=$1}
    in {region; value}
  }
| attributes field_name {
    let region = $2#region
    and value = Punned {pun=(E_Var $2); attributes=$1}
    in {region; value} }

(* The rule [compound] derives some of the most common compound
   constructs of PascaLIGO, typically definitions by extension of
   lists, sets and maps. Those constructs can be empty, for instance,
   an empty set makes sense. Following the writing guidelines here, we
   write first the terse version. The first parameter [Kind] is the
   keyword that determines the sort of definition: "set", "list",
   "map" or "big_map". *)

compound(Kind,element):
  terse_compound(Kind,element) | verb_compound(Kind,element) { $1 }

terse_compound(Kind,element):
  attributes Kind "[" ioption(sep_or_term_list(element,";")) "]" {
    let kind, enclosing = $2, Brackets ($3,$5)
    and elements, terminator =
      match $4 with
        Some (elts, term) -> Some elts, term
      |              None -> None, None in
    let region = cover $2#region $5#region
    and value  = {kind; enclosing; elements; terminator; attributes=$1}
    in {region; value} }

verb_compound(Kind,element):
  attributes Kind ioption(sep_or_term_list(element,";")) "end" {
    let kind, enclosing = $2, End $4
    and elements, terminator =
      match $3 with
        Some (elts, term) -> Some elts, term
      |              None -> None, None in
    let region = cover $2#region $4#region
    and value  = {kind; enclosing; elements; terminator; attributes=$1}
    in {region; value} }

(* Constructed expressions *)

ctor_app_expr:
  ctor par(nsepseq(expr,",")) {
    mk_reg (cover $1#region $2.region) (E_Ctor $1, Some $2) }
| ctor { {region=$1#region; value = (E_Ctor $1, None)} }

(* Tuples of expressions *)

tuple_expr: par(tuple(expr)) { $1 }

tuple(item):
  item "," nsepseq(item,",") { Utils.nsepseq_cons $1 $2 $3 }

(* Function calls *)

call_expr:
  path_expr arguments {
    let region = cover (expr_to_region) $2.region
    in mk_reg region ($1,$2) }

arguments: par(nsepseq(expr,",")) { $1 }

(* Typed expressions *)

typed_expr: disj_expr_level type_annotation { $1,$2 }

(* Path expressions

   A path expression is a construct that qualifies unambiguously a
   value or type. When maintaining this subgrammar, be wary of not
   introducing a regression. Here are the currently possible cases:

      * a single variable: "a" or "@0" or "@let" etc.
      * a single variable in a nested module: "A.B.a"
      * nested fields and compoments from a variable: "a.0.1.b"
      * same withing a nested module: "A.B.a.0.1.b"
      * nested fields and components from an expression: "(e).a.0.1.b"
 *)

path_expr:
  qualified_expr | local_path { $1 }

qualified_expr:
  module_path(field_path)
| module_path("or"  { E_Var $1 })
| module_path("and" { E_Var $1 }) {
    E_ModPath (mk_mod_path $1 expr_to_region) }

field_path:
  record_or_tuple "." nsepseq(selection,".") {
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover $1#region stop
    and value  = {record_or_tuple=(E_Var $1); selector=$2;
                  field_path=$3}
    in E_Proj {region; value}
  }
| variable  { E_Var $1 }
| par(expr) { E_Par $1 }

selection:
  field_name { FieldName $1 } (* Can be a component, e.g. "@1" *)
| "<int>"    { Component $1 }

local_path:
  par(expr) "." nsepseq(selection,".") {
    let record_or_tuple = E_Par $1 in
    let stop   = nsepseq_to_region selection_to_region $3 in
    let region = cover $1.region stop
    and value  = {record_or_tuple; selector=$2; field_path=$3}
    in E_Proj {region; value}
  }
| field_path { $1 }

(* Map lookups

   With the first rule of [map_lookup], all the above paths (rule
   [path_expr]) can be completed with an indexation, for example:

      * "a[i]"
      * "A.B.a[i]"
      * "a.0.1.b[i]"
      * "A.B.a.0.1.b[i]"

   and we have a second rule that adds the possibility to lookup a
   dynamically computed value, without introducing an intermediary
   variable:

      * "(e)[i]"
*)

map_lookup:
  path_expr brackets(expr) {
    let region = cover (expr_to_region $1) $2.region
    and value  = {map=$1; index=$2}
    in E_MapLookup {region; value} }

(* Updates *)

update:
  core_expr "with" core_expr {
    let start  = expr_to_region $1
    and stop   = expr_to_region $3 in
    let region = cover start stop
    and value  = {structure=$1; kwd_with=$2; update=$3}
    in {region; value} }

(* Code injection *)

code_inj:
  "[%lang" expr "]" {
    let region = cover $1.region $3#region
    and value  = {language=$1; code=$2; rbracket=$3}
    in {region; value} }

(* PATTERNS *)

pattern:
  core_pattern "#" pattern {
    let start  = pattern_to_region $1
    and stop   = pattern_to_region $3 in
    let region = cover start stop in
    P_Cons {region; value = $1,$2,$3}
  }
| core_pattern { $1 }

core_pattern:
  "<int>"            { P_Int     $1 }
| "<nat>"            { P_Nat     $1 }
| "<bytes>"          { P_Bytes   $1 }
| "<string>"         { P_String  $1 }
| "<mutez>"          { P_Mutez   $1 }
| "nil"              { P_Nil     $1 }
| var_pattern        { P_Var     $1 }
| list_pattern       { P_List    $1 }
| ctor_app_pattern   { P_App     $1 }
| tuple_pattern      { P_Tuple   $1 }
| record_pattern     { P_Record  $1 }
| qualified_pattern  { $1 }
| par(pattern)
| par(typed_pattern) { P_Par     $1 }

(**)

qualified_pattern:
  pattern_in_module(ctor { P_Ctor $1 }) tuple_pattern {
    let region = cover (pattern_to_region $1) $2.region
    and value  = $1, Some $2
    in P_App {region; value}
  }
| pattern_in_module(var_pattern  { P_Var $1 })
| pattern_in_module(par(pattern) { P_Par $1 }) { $1 }

pattern_in_module(pattern):
  module_path(pattern) {
    P_ModPath (mk_mod_path $1 pattern_to_region) }

(* List patterns *)

list_pattern:
  compound("list",core_pattern) { $1 }

(* Tuple pattern *)

tuple_pattern: par(tuple(pattern)) { $1 }

(* Constructed patterns

   Note that we do not use [tuple_pattern] because tuples must have at
   least two components. *)

ctor_app_pattern:
  ctor par(nsepseq(pattern,",")) {
    mk_reg (cover $1#region $2.region) (P_Ctor $1, Some $2) }
| ctor { {region=$1#region; value = (P_Ctor $1, None)} }

(* Record patterns *)

record_pattern:
  compound("record",field_pattern) { $1 }

field_pattern:
  attributes field_name "=" core_pattern {
    let region = cover $2#region (pattern_to_region $4)
    and value  = Complete {field_lhs=$2; assign=$3;
                           field_rhs=$4; attributes=$1}
    in {region; value}
  }
| attributes field_name {
    let value = Punned {pun=$2; attributes=$1}
    in {region = $2#region; value}
  }
| "_" {
    let value = Punned {pun=$1; attributes=[]}
    in {region=$1#region; value} }

(* Typed patterns *)

typed_pattern:
  pattern type_annotation {
    let start  = pattern_to_region $1
    and stop   = type_expr_to_region (snd $2) in
    let region = cover start stop
    and value  = {pattern=$1; type_annot=$2}
    in P_Typed {region; value} }
