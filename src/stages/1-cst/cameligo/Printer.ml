[@@@coverage exclude_file]

open CST

module Directive = LexerLib.Directive
module Region = Simple_utils.Region
open! Region
module Utils = Simple_utils.Utils

let sprintf = Printf.sprintf

type state = <
  offsets  : bool;
  mode     : [`Point | `Byte];
  buffer   : Buffer.t;
  pad_path : string;
  pad_node : string;
  pad      : int -> int -> state
>

let mk_state ~offsets ~mode ~buffer =
  object
    method offsets  = offsets;
    method mode     = mode;
    method buffer   = buffer
    val pad_path    = ""
    method pad_path = pad_path
    val pad_node    = ""
    method pad_node = pad_node

    (* The method [pad] updates the current padding, which is
       comprised of two components: the padding to reach the new node
       (space before reaching a subtree, then a vertical bar for it)
       and the padding for the new node itself (Is it the last child
       of its parent?).

       A child node that is not the last satisfies [rank < arity] and
       the last child satisfies [rank = arity], where the rank of the
       first child is 0. *)

    method pad arity rank =
      {< pad_path =
           pad_node ^ (if rank = arity-1 then "`-- " else "|-- ");
         pad_node =
           pad_node ^ (if rank = arity-1 then "    " else "|   ")
      >}
  end

let compact state (region: Region.t) =
  region#compact ~offsets:state#offsets state#mode

(* Printing the tokens with their source regions *)

let print_option : state -> (state -> 'a -> unit) -> 'a option -> unit =
  fun state print -> function
          None -> ()
  | Some node -> print state node

let print_nsepseq :
  state -> (state -> 'a -> unit) -> string -> ('a, region) Utils.nsepseq -> unit =
  fun state print sep (head, tail) ->
    let print_aux (sep_reg, item) =
      let sep_line =
        sprintf "%s: %s\n" (compact state sep_reg) sep in
      Buffer.add_string state#buffer sep_line;
      print state item
    in print state head; List.iter print_aux tail

let print_sepseq :
  state -> (state -> 'a -> unit) -> string -> ('a, region) Utils.sepseq -> unit =
  fun state print sep ->
    print_option state (fun state -> print_nsepseq state print sep)

let print_nseq : state -> (state -> 'a -> unit) -> 'a Utils.nseq -> unit =
  fun state print -> Utils.nseq_iter (print state)

let print_csv state print (node : ('a, comma) Utils.nsepseq reg) =
  print_nsepseq state print "," node.value

let print_markup state c =
  let markup = match c with
    LineCom (c, _) -> sprintf "// %s" c.value
  | BlockCom (c, _) -> sprintf "(* %s *)" c.value
  in
  Buffer.add_string state#buffer markup

let print_markup state comments =
  List.iter (print_markup state) comments

let print_token state region lexeme =
  print_markup state region#markup;
  let line =
    sprintf "%s: %s\n" (compact state region) lexeme
  in Buffer.add_string state#buffer line

let print_token_opt state lexeme =
  print_option state (fun state -> print_token state lexeme)

let print_var state {region; value} =
  let line =
    sprintf "%s: Ident %s\n"
            (compact state region)value
  in Buffer.add_string state#buffer line

let print_ctor state {region; value} =
  let line =
    sprintf "%s: Ctor \"%s\"\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_pvar state {region; value} =
  let line =
    sprintf "%s: PVar %s\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_string state {region; value} =
  let line =
    sprintf "%s: String %S\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_verbatim state {region; value} =
  let line =
    sprintf "%s: Verbatim %S\n"
            (compact state region) value
  in Buffer.add_string state#buffer line

let print_bytes state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Bytes (\"%s\", \"0x%s\")\n"
            (compact state region) lexeme
            (Hex.show abstract)
  in Buffer.add_string state#buffer line

let print_int state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Int (\"%s\", %s)\n"
            (compact state region) lexeme
            (Z.to_string abstract)
  in Buffer.add_string state#buffer line

let print_nat state {region; value} =
  let lexeme, abstract = value in
  let line =
    sprintf "%s: Nat (\"%s\", %s)\n"
            (compact state region) lexeme
            (Z.to_string abstract)
  in Buffer.add_string state#buffer line

let print_mutez state {region; value=lex,z} =
  let line =
    sprintf "Mutez %s (%s)" lex (Z.to_string z)
  in print_token state line region

let rec print_tokens state (node : cst) =
  print_nseq  state print_declaration node.decl;
  print_token state "EOF" node.eof

and print_attributes state (node : attribute list) =
  let apply {value; region} =
    let attribute_formatted = sprintf "[@%s]" value in
    print_token state attribute_formatted region
  in List.iter apply node

and print_declaration state = function
  Directive   d -> print_directive    state d
| Let         d -> print_let_decl     state d
| TypeDecl    d -> print_type_decl    state d
| ModuleDecl  d -> print_module_decl  state d
| ModuleAlias d -> print_module_alias state d

and print_directive state dir =
  let s =
    Directive.to_string ~offsets:state#offsets state#mode dir
  in Buffer.add_string state#buffer s

and print_let_decl state (node : let_decl reg) =
  let kwd_let, kwd_rec, let_binding, attributes = node.value in
  print_attributes   state attributes;
  print_token        state "let" kwd_let;
  print_token_opt    state "rec" kwd_rec;
  print_let_binding  state let_binding

and print_type_decl state (node : type_decl reg) =
  print_token     state "type" node.value.kwd_type;
  print_var       state node.value.name;
  print_token     state "=" node.value.eq;
  print_type_expr state node.value.type_expr

and print_module_decl state (node : module_decl reg) =
  print_token  state "module" node.value.kwd_module;
  print_var    state node.value.name;
  print_token  state "=" node.value.eq;
  print_token  state "struct" node.value.kwd_struct;
  print_tokens state node.value.structure;
  print_token  state "end" node.value.kwd_end

and print_module_alias state (node : module_alias reg) =
  print_token   state "module" node.value.kwd_module;
  print_var     state node.value.alias;
  print_token   state "=" node.value.eq;
  print_nsepseq state print_var "." node.value.mod_path

and print_type_expr state = function
  TProd   t -> print_cartesian   state t
| TSum    t -> print_sum_type    state t
| TRecord t -> print_record_type state t
| TApp    t -> print_type_app    state t
| TPar    t -> print_type_par    state t
| TVar    t -> print_var         state t
| TWild   t -> print_token       state "_" t
| TFun    t -> print_fun_type    state t
| TString t -> print_string      state t
| TInt    t -> print_int         state t
| TModA   t -> print_mod_access  state print_type_expr t

and print_sum_type state (node : sum_type reg) =
  print_attributes state node.value.attributes;
  print_token_opt  state "|" node.value.lead_vbar;
  print_nsepseq    state print_variant "|" node.value.variants

and print_fun_type state (node : (type_expr * arrow * type_expr) reg) =
  let domain, arrow, range = node.value in
  print_type_expr state domain;
  print_token     state "->" arrow;
  print_type_expr state range

and print_type_app state (node : (type_ctor * type_tuple) reg) =
  let type_ctor, type_tuple = node.value in
  print_type_tuple state type_tuple;
  print_var        state type_ctor

and print_type_tuple state (node : type_tuple) =
  let print state = print_nsepseq state print_type_expr ","
  in print_par state print node.value

and print_type_par state (node : type_expr par reg) =
  print_par state print_type_expr node.value

and print_projection state (node : projection reg) =
  print_var     state node.value.struct_name;
  print_token   state "." node.value.selector;
  print_nsepseq state print_selection "." node.value.field_path

and print_mod_access :
  'a.state -> (state -> 'a -> unit ) -> 'a module_access reg -> unit =
  fun state print node ->
    print_var   state node.value.module_name;
    print_token state "." node.value.selector;
    print       state node.value.field

and print_update state (node : update reg) =
  print_token        state "{" node.value.lbrace;
  print_path         state node.value.record;
  print_token        state "with" node.value.kwd_with;
  print_ne_injection state print_field_path_assignment node.value.updates;
  print_token        state "}" node.value.rbrace

and print_path state = function
  Name var  -> print_var        state var
| Path path -> print_projection state path

and print_selection state = function
  FieldName id -> print_var state id
| Component c  -> print_int state c

and print_cartesian state (node : (type_expr, times) Utils.nsepseq reg) =
  print_nsepseq state print_type_expr "*" node.value

and print_of_type_expr state (kwd_of, t_expr) =
  print_token     state "of" kwd_of;
  print_type_expr state t_expr

and print_variant state (node : variant reg) =
  print_attributes state node.value.attributes;
  print_ctor       state node.value.ctor;
  print_option     state print_of_type_expr node.value.arg

and print_field_decl state (node : field_decl reg) =
  print_attributes state node.value.attributes;
  print_var        state node.value.field_name;
  print_token      state ":" node.value.colon;
  print_type_expr  state node.value.field_type

and print_injection :
  'a.state -> (state -> 'a -> unit) -> 'a injection reg -> unit =
  fun state print node ->
    print_option state print_open_compound node.value.compound;
    print_sepseq state print ";" node.value.elements;
    print_option state print_semi node.value.terminator;
    print_option state print_close_compound node.value.compound

and print_ne_injection :
  'a.state -> (state -> 'a -> unit) -> 'a ne_injection reg -> unit =
  fun state print node ->
    print_attributes state node.value.attributes;
    print_option     state print_open_compound node.value.compound;
    print_nsepseq    state print ";" node.value.ne_elements;
    print_option     state print_semi node.value.terminator;
    print_option     state print_close_compound node.value.compound

and print_record_type state =
  print_ne_injection state print_field_decl

and print_open_compound state = function
  BeginEnd (kwd_begin, _) -> print_token state "begin" kwd_begin
| Braces   (lbrace, _)    -> print_token state "{"     lbrace
| Brackets (lbracket, _)  -> print_token state "["     lbracket

and print_close_compound state = function
  BeginEnd (_, kwd_end)  -> print_token state "end" kwd_end
| Braces   (_, rbrace)   -> print_token state "}"   rbrace
| Brackets (_, rbracket) -> print_token state "]"   rbracket

and print_semi state = print_token state ";"

and print_type_annot state (colon, type_expr) =
  print_token     state ":" colon;
  print_type_expr state type_expr

and print_let_binding state (node : let_binding) =
  print_nseq   state print_pattern node.binders;
  print_option state print_type_annot node.lhs_type;
  print_token  state "=" node.eq;
  print_expr   state node.let_rhs

and print_par : 'a.state -> (state -> 'a -> unit) -> 'a par -> unit =
  fun state print node ->
    print_token state "(" node.lpar;
    print       state node.inside;
    print_token state ")" node.rpar

and print_pattern state = function
  PTuple    p -> print_csv            state print_pattern p
| PList     p -> print_list_pattern   state p
| PVar      p -> print_pvar           state p
| PInt      p -> print_int            state p
| PNat      p -> print_nat            state p
| PBytes    p -> print_bytes          state p
| PString   p -> print_string         state p
| PVerbatim p -> print_verbatim       state p
| PWild     p -> print_token          state "_" p
| PCtor     p -> print_ctor_pattern   state p
| PRecord   p -> print_record_pattern state p
| PTyped    p -> print_typed_pattern  state p
| PUnit     p -> print_unit           state p
| PPar      p -> print_par            state print_pattern p.value

and print_list_pattern state = function
  PListComp p -> print_injection state print_pattern p
| PCons     p -> print_raw       state p

and print_raw state (node : (pattern * cons * pattern) reg) =
  let head, cons, tail = node.value in
  print_pattern state head;
  print_token   state "::" cons;
  print_pattern state tail

and print_typed_pattern state (node : typed_pattern reg) =
  print_pattern   state node.value.pattern;
  print_token     state ":" node.value.colon;
  print_type_expr state node.value.type_expr

and print_record_pattern state =
  print_ne_injection state print_field_pattern

and print_field_pattern state (node : field_pattern reg) =
  print_var     state node.value.field_name;
  print_token   state "=" node.value.eq;
  print_pattern state node.value.pattern

and print_ctor_pattern state = function
  PNone    p -> print_none_pattern     state p
| PSomeApp p -> print_psome_app state p
| PFalse   p -> print_token            state "false" p
| PTrue    p -> print_token            state "true" p
| PCtorApp p -> print_pctor_app state p

and print_none_pattern state = print_token state "None"

and print_psome_app state (node : (kwd_Some * pattern) reg) =
  let ctor_Some, argument = node.value in
  print_token   state "Some" ctor_Some;
  print_pattern state argument

and print_pctor_app state (node : (ctor * pattern option) reg) =
  let ctor, param = node.value in
  print_ctor   state ctor;
  print_option state print_pattern param

and print_expr state = function
  ELetIn    e -> print_let_in      state e
| ETypeIn   e -> print_type_in     state e
| EModIn    e -> print_mod_in      state e
| EModAlias e -> print_mod_alias   state e
| ECond     e -> print_conditional state e
| ETuple    e -> print_csv         state print_expr e
| ECase     e -> print_match_expr  state e
| EFun      e -> print_fun_expr    state e
| EAnnot    e -> print_annot_expr  state e
| ELogic    e -> print_logic_expr  state e
| EArith    e -> print_arith_expr  state e
| EString   e -> print_string_expr state e
| ECall     e -> print_fun_call    state e
| EVar      e -> print_var         state e
| EProj     e -> print_projection  state e
| EModA     e -> print_mod_access  state print_expr e
| EUpdate   e -> print_update      state e
| EUnit     e -> print_unit        state e
| EBytes    e -> print_bytes       state e
| EPar      e -> print_par_expr    state e
| EList     e -> print_list_expr   state e
| ESeq      e -> print_sequence    state e
| ERecord   e -> print_record_expr state e
| ECtor     e -> print_ctor_expr   state e
| ECodeInj  e -> print_code_inj    state e

and print_ctor_expr state = function
  ENone    e -> print_none_expr     state e
| ESomeApp e -> print_some_app_expr state e
| ECtorApp e -> print_ctor_app_expr state e

and print_none_expr state = print_token state "None"

and print_some_app_expr state (node : (kwd_Some * expr) reg) =
  let ctor_Some, argument = node.value in
  print_token state "Some" ctor_Some;
  print_expr  state argument

and print_ctor_app_expr state (node : (ctor * expr option) reg) =
  let ctor, argument = node.value in
  print_ctor   state ctor;
  print_option state print_expr argument

and print_par_expr state (node : expr par reg) =
  print_par state print_expr node.value

and print_unit state (node : the_unit reg) =
  let lpar, rpar = node.value in
  print_token state "(" lpar;
  print_token state ")" rpar

and print_fun_call state (node : (expr * expr Utils.nseq) reg) =
  let func, args = node.value in
  print_expr state func;
  print_nseq state print_expr args

and print_annot_expr state (node : annot_expr par reg) =
  let print state (expr, colon, type_expr) =
    print_expr      state expr;
    print_token     state ":" colon;
    print_type_expr state type_expr
  in print_par state print node.value

and print_list_expr state = function
    ECons   e -> print_op2 state "::" e
| EListComp e -> if   e.value.elements = None
                then print_token state "[]" e.region
                else print_injection state print_expr e

and print_op1 state lexeme (node : keyword un_op reg) =
  print_token state lexeme node.value.op;
  print_expr  state node.value.arg

and print_op2 state lexeme (node : keyword bin_op reg) =
  print_expr  state node.value.arg1;
  print_token state lexeme node.value.op;
  print_expr  state node.value.arg2

and print_arith_expr state = function
  Add   e -> print_op2   state "+" e
| Sub   e -> print_op2   state "-" e
| Mult  e -> print_op2   state "*" e
| Div   e -> print_op2   state "/" e
| Mod   e -> print_op2   state "mod" e
| Neg   e -> print_op1   state "-" e
| Int   e -> print_int   state e
| Nat   e -> print_nat   state e
| Mutez e -> print_mutez state e

and print_string_expr state = function
  Cat      e -> print_op2      state "^" e
| String   e -> print_string   state e
| Verbatim e -> print_verbatim state e

and print_logic_expr state = function
  BoolExpr e -> print_bool_expr state e
| CompExpr e -> print_comp_expr state e

and print_bool_expr state = function
  Or    e -> print_op2   state "||"    e
| And   e -> print_op2   state "&&"    e
| Not   e -> print_op1   state "not"   e
| True  e -> print_token state "true"  e
| False e -> print_token state "false" e

and print_comp_expr state = function
  Lt    e -> print_op2 state "<"  e
| Leq   e -> print_op2 state "<=" e
| Gt    e -> print_op2 state ">"  e
| Geq   e -> print_op2 state ">=" e
| Neq   e -> print_op2 state "<>" e
| Equal e -> print_op2 state "="  e

and print_record_expr state =
  print_ne_injection state print_field_assignment

and print_code_inj state (node : code_inj reg) =
  let {value; region} = node.value.language in
  let header_stop = region#start#shift_bytes 1 in
  let header_reg  = Region.make ~start:region#start ~stop:header_stop in
  print_token  state "[%" header_reg;
  print_string state value;
  print_expr   state node.value.code;
  print_token  state "]" node.value.rbracket

and print_field_assignment state (node : field_assignment reg) =
  print_var   state node.value.field_name;
  print_token state "=" node.value.assignment;
  print_expr  state node.value.field_expr

and print_field_path_assignment state {value; _} =
  let {field_path; assignment; field_expr} = value in
  print_path  state field_path;
  print_token state "=" assignment;
  print_expr  state field_expr

and print_sequence state seq =
  print_injection state print_expr seq

and print_match_expr state {value; _} =
  let {kwd_match; expr; kwd_with; lead_vbar; cases} = value in
  print_token     state "match" kwd_match ;
  print_expr      state expr;
  print_token     state "with" kwd_with;
  print_token_opt state "|" lead_vbar;
  print_cases     state cases

and print_cases state {value; _} =
  print_nsepseq state print_case_clause "|" value

and print_case_clause state {value; _} =
  let {pattern; arrow; rhs} = value in
  print_pattern state pattern;
  print_token   state "->" arrow;
  print_expr    state rhs

and print_let_in state {value; _} =
  let {kwd_let; kwd_rec; binding; kwd_in; body; attributes} = value in
  print_attributes   state attributes;
  print_token        state "let" kwd_let ;
  print_token_opt    state "rec" kwd_rec;
  print_let_binding  state binding;
  print_token        state "in" kwd_in;
  print_expr         state body

and print_type_in state {value; _} =
  let {type_decl; kwd_in; body} = value in
  let {kwd_type; name; eq; type_expr} = type_decl in
  print_token     state "type" kwd_type;
  print_var       state name;
  print_token     state "eq" eq;
  print_type_expr state type_expr;
  print_token     state "in" kwd_in;
  print_expr      state body

and print_mod_in state {value; _} =
  let {mod_decl; kwd_in; body} = value in
  let {kwd_module; name; eq; kwd_struct; structure; kwd_end} = mod_decl in
  print_token  state "module" kwd_module;
  print_var    state name;
  print_token  state "eq" eq;
  print_token  state "struct" kwd_struct;
  print_tokens state structure;
  print_token  state "end" kwd_end;
  print_token  state "in" kwd_in;
  print_expr   state body

and print_mod_alias state {value; _} =
  let {mod_alias; kwd_in; body} = value in
  let {kwd_module; alias; eq; mod_path} = mod_alias in
  print_token   state "module" kwd_module;
  print_var     state alias;
  print_token   state "eq" eq;
  print_nsepseq state print_var "." mod_path;
  print_token   state "in" kwd_in;
  print_expr    state body

and print_fun_expr state {value; _} =
  let {kwd_fun; binders; lhs_type; arrow; body} = value in
  print_token  state "fun" kwd_fun;
  print_nseq   state print_pattern binders;
  print_option state print_type_annot lhs_type;
  print_token  state "->" arrow;
  print_expr   state body

and print_else state (kwd_else, ifnot) =
  print_token state "else" kwd_else;
  print_expr  state ifnot

and print_conditional state {value; _} =
  let {kwd_if; test; kwd_then; ifso; ifnot} = value in
  print_token  state "(" ghost;
  print_token  state "if" kwd_if ;
  print_expr   state test;
  print_token  state "then" kwd_then;
  print_expr   state ifso;
  print_option state print_else ifnot;
  print_token  state ")" ghost

(* Conversion to string *)

let to_string ~offsets ~mode print node =
  let buffer = Buffer.create 131 in
  let state = mk_state ~offsets ~mode ~buffer in
  let () = print state node
  in Buffer.contents buffer

let tokens_to_string = to_string print_tokens

let pattern_to_string = to_string print_pattern

let expr_to_string = to_string print_expr

let type_expr_to_string = to_string print_type_expr

(* Pretty-printing the CST *)

let pp_ident state {value=name; region} =
  let reg  = compact state region in
  let node = sprintf "%s%s (%s)\n" state#pad_path name reg
  in Buffer.add_string state#buffer node

let pp_node state name =
  let node = sprintf "%s%s\n" state#pad_path name
  in Buffer.add_string state#buffer node

let pp_string state {value=name; region} =
  let reg  = compact state region in
  let node = sprintf "%s%S (%s)\n" state#pad_path name reg
  in Buffer.add_string state#buffer node

let pp_verbatim state {value=name; region} =
  let reg  = compact state region in
  let node = sprintf "%s{|%s|} (%s)\n" state#pad_path name reg
  in Buffer.add_string state#buffer node

let pp_markup_item state c =
  let comment = match c with
    LineCom (c, _) -> sprintf "//%s" c.value
  | BlockCom (c, _) -> sprintf "(*%s*)" c.value
  in
  Buffer.add_string state#buffer comment

let pp_markup state comments =
  List.iter (pp_markup_item state) comments

let pp_loc_node state name region =
  pp_markup state region#markup;
  pp_ident state {value=name; region}

let rec pp_cst state {decl; _} =
  let apply len rank = pp_declaration (state#pad len rank) in
  let decls = Utils.nseq_to_list decl in
  pp_node state "<cst>";
  List.iteri (List.length decls |> apply) decls

and pp_declaration state = function
  Let {value = (kwd_let, kwd_rec, let_binding, attr); region} ->
    pp_markup state kwd_let#markup;
    pp_loc_node state "Let" region;
    (if kwd_rec <> None then pp_node (state#pad 0 0) "rec"); (* Hack *)
    pp_let_binding state let_binding attr
| TypeDecl {value; region} ->
    pp_loc_node  state "TypeDecl" region;
    pp_type_decl state value
| ModuleDecl {value; region} ->
    pp_loc_node    state "ModuleDecl" region;
    pp_module_decl state value
| ModuleAlias {value; region} ->
    pp_loc_node     state "ModuleDecl" region;
    pp_module_alias state value
| Directive dir ->
    let region, string = Directive.project dir in
    pp_loc_node state "Directive" region;
    pp_node state string

and pp_let_binding state node attr =
  let {binders; lhs_type; let_rhs; _} = node in
  let arity = if lhs_type = None then 2 else 3 in
  let arity = if attr = [] then arity else arity+1 in
  let rank =
    let state = state#pad arity 0 in
    pp_node    state "<binders>";
    pp_binders state binders; 0 in
  let rank =
    match lhs_type with
      None -> rank
    | Some (_, type_expr) ->
       let state = state#pad arity (rank+1) in
       pp_node state "<lhs type>";
       pp_type_expr (state#pad 1 0) type_expr;
       rank+1 in
  let rank =
    let state = state#pad arity (rank+1) in
    pp_node state "<rhs>";
    pp_expr (state#pad 1 0) let_rhs;
    rank+1 in
  if attr <> [] then
    let state = state#pad arity (rank+1) in
    pp_node state "<attributes>";
    let length         = List.length attr in
    let apply len rank = pp_ident (state#pad len rank)
    in List.iteri (apply length) attr

and pp_type_decl state decl =
  pp_ident     (state#pad 2 0) decl.name;
  pp_type_expr (state#pad 2 1) decl.type_expr

and pp_module_decl state decl =
  pp_ident (state#pad 2 0) decl.name;
  pp_cst   (state#pad 2 1) decl.structure

and pp_module_alias state decl =
  let mod_path       = Utils.nsepseq_to_list decl.mod_path in
  let len            = List.length mod_path in
  let apply len rank = pp_ident (state#pad len rank) in
  pp_ident (state#pad (1+len) 0) decl.alias;
  List.iteri (apply len) mod_path

and pp_binders state patterns =
  let patterns       = Utils.nseq_to_list patterns in
  let arity          = List.length patterns in
  let apply len rank = pp_pattern (state#pad len rank)
  in List.iteri (apply arity) patterns

and pp_pattern state = function
  PCtor p ->
    pp_node         state "PCtor";
    pp_ctor_pattern (state#pad 1 0) p
| PVar p ->
    let {var; attributes} = p in
    pp_node state "PVar";
    let arity = if attributes = [] then 1 else 2 in
    pp_ident (state#pad 1 0) var;
    if attributes <> [] then
      pp_attributes (state#pad arity 1) attributes
| PInt p ->
    pp_node state "PInt";
    pp_int  state p
| PNat p ->
    pp_node state "PNat";
    pp_int  state p
| PBytes p ->
    pp_node  state "PBytes";
    pp_bytes state p
| PString p ->
    pp_node   state "PString";
    pp_string (state#pad 1 0) p
| PVerbatim p ->
    pp_node     state "PVerbatim";
    pp_verbatim (state#pad 1 0) p
| PUnit {region; _} ->
    pp_loc_node state "PUnit" region
| PList p ->
    pp_node         state "PList";
    pp_list_pattern (state#pad 1 0) p
| PTuple {region; value} ->
    pp_loc_node      state "PTuple" region;
    pp_tuple_pattern (state#pad 1 0) value
| PPar {value; _} ->
    pp_node    state "PPar";
    pp_pattern (state#pad 1 0) value.inside
| PRecord {value; _} ->
    pp_node         state "PRecord";
    pp_ne_injection state pp_field_pattern value
| PTyped {value; _} ->
    pp_node          state "PTyped";
    pp_typed_pattern state value

and pp_field_pattern state {value; _} =
  pp_node    state value.field_name.value;
  pp_pattern (state#pad 1 0) value.pattern

and pp_typed_pattern state node =
  pp_pattern   (state#pad 2 0) node.pattern;
  pp_type_expr (state#pad 2 1) node.type_expr

and pp_tuple_pattern state tuple =
  let patterns       = Utils.nsepseq_to_list tuple in
  let length         = List.length patterns in
  let apply len rank = pp_pattern (state#pad len rank)
  in List.iteri (apply length) patterns

and pp_list_pattern state = function
  PCons {value; region} ->
    let pat1, _, pat2 = value in
    pp_loc_node state "PCons" region;
    pp_pattern  (state#pad 2 0) pat1;
    pp_pattern  (state#pad 2 1) pat2
| PListComp {value; region} ->
    pp_loc_node state "PListComp" region;
    if value.elements = None
    then pp_node (state#pad 1 0) "<nil>"
    else pp_injection state pp_pattern value

and pp_injection :
  'a.state -> (state -> 'a -> unit) -> 'a injection -> unit =
  fun state print inj ->
    let elements       = Utils.sepseq_to_list inj.elements in
    let length         = List.length elements in
    let apply len rank = print (state#pad len rank)
    in List.iteri (apply length) elements

and pp_ne_injection :
  'a.state -> (state -> 'a -> unit) -> 'a ne_injection -> unit =
  fun state print inj ->
    let ne_elements = Utils.nsepseq_to_list inj.ne_elements in
    let length      = List.length ne_elements in
    let arity       = if inj.attributes = [] then length else length+1
    and apply len rank = print (state#pad len rank)
    in List.iteri (apply arity) ne_elements;
       if inj.attributes <> [] then
         let state = state#pad arity (arity-1)
         in pp_attributes state inj.attributes

and pp_record_type state = pp_ne_injection state pp_field_decl

and pp_bytes state {value=lexeme,hex; region} =
  pp_loc_node (state#pad 2 0) lexeme region;
  pp_node     (state#pad 2 1) (Hex.show hex)

and pp_int state {value=lexeme,z; region} =
  pp_loc_node (state#pad 2 0) lexeme region;
  pp_node     (state#pad 2 1) (Z.to_string z)

and pp_ctor_pattern state = function
  PNone region ->
    pp_loc_node state "PNone" region
| PSomeApp {value=_,param; region} ->
    pp_loc_node state "PSomeApp" region;
    pp_pattern  (state#pad 1 0) param
| PFalse region ->
    pp_loc_node state "PFalse" region
| PTrue region ->
    pp_loc_node state "PTrue" region
| PCtorApp {value; region} ->
    pp_loc_node state "PCtorApp" region;
    pp_ctor_app_pattern (state#pad 1 0) value

and pp_ctor_app_pattern state (ctor, pat_opt) =
  pp_ident state ctor;
  print_option state pp_pattern pat_opt

and pp_expr state = function
  ECase {value; region} ->
    pp_loc_node state "ECase" region;
    pp_case     state pp_expr value
| ECond {value; region} ->
    pp_loc_node state "ECond" region;
    pp_cond_expr state value
| EAnnot {value; region} ->
    pp_loc_node  state "EAnnot" region;
    pp_annotated state value
| ELogic e_logic ->
    pp_node state "ELogic";
    pp_e_logic (state#pad 1 0) e_logic
| EArith e_arith ->
    pp_node state "EArith";
    pp_arith_expr (state#pad 1 0) e_arith
| EString e_string ->
    pp_node state "EString";
    pp_string_expr (state#pad 1 0) e_string
| EList e_list ->
    pp_node state "EList";
    pp_list_expr (state#pad 1 0) e_list
| ECtor e_ctor ->
    pp_node state "ECtor";
    pp_ctor_expr (state#pad 1 0) e_ctor
| ERecord {value; region} ->
    pp_loc_node     state "ERecord" region;
    pp_ne_injection state pp_field_assignment value
| EProj {value; region} ->
    pp_loc_node state "EProj" region;
    pp_projection state value
| EModA {value; region} ->
    pp_loc_node state "EModA" region;
    pp_module_access state pp_expr value
| EUpdate {value; region} ->
    pp_loc_node state "EUpdate" region;
    pp_update state value
| EVar v ->
    pp_node  state "EVar";
    pp_ident (state#pad 1 0) v
| ECall {value; region} ->
    pp_loc_node state "ECall" region;
    pp_fun_call state value
| EBytes b ->
    pp_node  state "EBytes";
    pp_bytes state b
| EUnit u ->
    pp_loc_node state "EUnit" u.region
| ETuple e_tuple ->
    pp_node       state "ETuple";
    pp_tuple_expr state e_tuple
| EPar {value; region} ->
    pp_loc_node state "EPar" region;
    pp_expr     (state#pad 1 0) value.inside
| ELetIn {value; region} ->
    pp_loc_node state  "ELetIn" region;
    pp_let_in   state value
| ETypeIn {value; region} ->
    pp_loc_node state  "ETypeIn" region;
    pp_type_in  state value
| EModIn {value; region} ->
    pp_loc_node state  "EModIn" region;
    pp_mod_in   state value
| EModAlias {value; region} ->
    pp_loc_node  state  "EModAlias" region;
    pp_mod_alias state value
| EFun {value; region} ->
    pp_loc_node state "EFun" region;
    pp_fun_expr state value
| ESeq {value; region} ->
    pp_loc_node  state "ESeq" region;
    pp_injection state pp_expr value
| ECodeInj {value; region} ->
    pp_loc_node state "ECodeInj" region;
    pp_code_inj state value

and pp_fun_expr state node =
  let {binders; lhs_type; body; _} = node in
  let arity = if lhs_type = None then 2 else 3 in
  let () =
    let state = state#pad arity 0 in
    pp_node state "<parameters>";
    pp_binders state binders in
  let () =
    match lhs_type with
      None -> ()
    | Some (_, type_expr) ->
       let state = state#pad arity 1 in
       pp_node state "<lhs type>";
       pp_type_expr (state#pad 1 0) type_expr in
  let () =
    let state = state#pad arity (arity - 1) in
    pp_node state "<body>";
    pp_expr (state#pad 1 0) body
  in ()

and pp_code_inj state rc =
  let () =
    let state = state#pad 2 0 in
    pp_node state "<language>";
    pp_string (state#pad 1 0) rc.language.value in
  let () =
    let state = state#pad 2 1 in
    pp_node state "<code>";
    pp_expr (state#pad 1 0) rc.code
  in ()

and pp_let_in state node =
  let {binding; body; attributes; kwd_rec; _} = node in
  let {binders; lhs_type; let_rhs; _} = binding in
  let arity = if lhs_type = None then 3 else 4 in
  let arity = if kwd_rec = None then arity else arity+1 in
  let arity = if attributes = [] then arity else arity+1 in
  let rank =
    match kwd_rec with
        None -> 0
    | Some _ ->
        let state = state#pad arity 0 in
        pp_node state "rec"; 0 in
  let rank =
    let state = state#pad arity 0 in
    pp_node state "<binders>";
    pp_binders state binders; rank in
  let rank =
    match lhs_type with
      None -> rank
    | Some (_, type_expr) ->
       let state = state#pad arity (rank+1) in
       pp_node state "<lhs type>";
       pp_type_expr (state#pad 1 0) type_expr;
       rank+1 in
  let rank =
    let state = state#pad arity (rank+1) in
    pp_node state "<rhs>";
    pp_expr (state#pad 1 0) let_rhs;
    rank+1 in
  let rank =
    let state = state#pad arity (rank+1) in
    pp_node state "<body>";
    pp_expr (state#pad 1 0) body;
    rank+1 in
  let () =
    if attributes <> [] then
      let state = state#pad arity (rank+1) in
      pp_node state "<attributes>";
      let length         = List.length attributes in
      let apply len rank = pp_ident (state#pad len rank)
      in List.iteri (apply length) attributes
  in ()

and pp_type_in state node =
  let {type_decl; body; _} = node in
  let {name; type_expr; _} = type_decl in
  let () =
    let state = state#pad 3 0 in
    pp_node  state "<name>";
    pp_ident state name in
  let () =
    let state = state#pad 3 1 in
    pp_node      state "<type>";
    pp_type_expr (state#pad 1 0) type_expr in
  let () =
    let state = state#pad 3 2 in
    pp_node state "<body>";
    pp_expr (state#pad 1 0) body
  in ()

and pp_mod_in state node =
  let {mod_decl; body; _} = node in
  let {name; structure; _} = mod_decl in
  let () =
    let state = state#pad 3 0 in
    pp_node  state "<name>";
    pp_ident state name in
  let () =
    let state = state#pad 3 1 in
    pp_node state "<module>";
    pp_cst (state#pad 1 0) structure in
  let () =
    let state = state#pad 3 2 in
    pp_node state "<body>";
    pp_expr (state#pad 1 0) body
  in ()

and pp_mod_alias state node =
  let {mod_alias; body; _} = node in
  let {alias; mod_path; _} = mod_alias in
  let () =
    let state = state#pad 3 0 in
    pp_node  state "<alias>";
    pp_ident state alias in
  let () =
    let state          = state#pad 3 1 in
    let mod_path       = Utils.nsepseq_to_list mod_path in
    let len            = List.length mod_path in
    let apply len rank = pp_ident (state#pad len rank) in
    pp_node state "<module>";
    List.iteri (apply len) mod_path in
  let () =
    let state = state#pad 3 2 in
    pp_node state "<body>";
    pp_expr (state#pad 1 0) body
  in ()

and pp_attributes state attributes =
  pp_node state "<attributes>";
  let length         = List.length attributes in
  let apply len rank = pp_ident (state#pad len rank)
  in List.iteri (apply length) attributes

and pp_tuple_expr state {value; _} =
  let exprs          = Utils.nsepseq_to_list value in
  let length         = List.length exprs in
  let apply len rank = pp_expr (state#pad len rank)
  in List.iteri (apply length) exprs

and pp_fun_call state (fun_expr, args) =
  let args           = Utils.nseq_to_list args in
  let arity          = List.length args in
  let apply len rank = pp_expr (state#pad len rank)
  in pp_expr (state#pad (1+arity) 0) fun_expr;
     List.iteri (apply arity) args

and pp_projection state proj =
  let selections     = Utils.nsepseq_to_list proj.field_path in
  let len            = List.length selections in
  let apply len rank = pp_selection (state#pad len rank) in
  pp_ident (state#pad (1+len) 0) proj.struct_name;
  List.iteri (apply len) selections

and pp_module_access :
  type a.state -> (state -> a -> unit ) -> a module_access -> unit =
  fun state print access ->
    pp_ident (state#pad 2 0) access.module_name;
    print    (state#pad 2 1) access.field

and pp_update state {record; updates; _} =
  pp_path         (state#pad 2 0) record;
  pp_ne_injection state pp_field_path_assignment updates.value

and pp_path state = function
  Name name ->
    pp_node state "Name";
    pp_ident (state#pad 1 0) name
| Path {value; region} ->
    pp_loc_node state "Path" region;
    pp_projection state value

and pp_selection state = function
  FieldName fn ->
    pp_node state "FieldName";
    pp_ident (state#pad 1 0) fn
| Component c ->
    pp_node state "Component";
    pp_int state c

and pp_field_assignment state (node : field_assignment reg) =
  pp_node  state  "<field assignment>";
  pp_ident (state#pad 2 0) node.value.field_name;
  pp_expr  (state#pad 2 1) node.value.field_expr

and pp_field_path_assignment state (node : field_path_assignment reg) =
  pp_node state "<update>";
  pp_path (state#pad 2 0) node.value.field_path;
  pp_expr (state#pad 2 1) node.value.field_expr

and pp_ctor_expr state = function
  ENone    e -> pp_loc_node state "ENone" e
| ESomeApp e -> pp_some_app state e
| ECtorApp e -> pp_ctor_app state e

and pp_some_app state (node : (kwd_Some * expr) reg) =
  let _, arg = node.value in
  pp_loc_node state "ESomeApp" node.region;
  pp_expr     (state#pad 1 0) arg

and pp_ctor_app state (node : (ctor * expr option) reg) =
  let ctor, expr_opt = node.value in
  pp_loc_node state "ECtorApp" node.region;
  match expr_opt with
         None -> pp_ident (state#pad 1 0) ctor
  | Some expr -> pp_ident (state#pad 2 0) ctor;
                 pp_expr  (state#pad 2 1) expr

and pp_list_expr state = function
  ECons {value; region} ->
    pp_loc_node state "ECons" region;
    pp_expr     (state#pad 2 0) value.arg1;
    pp_expr     (state#pad 2 1) value.arg2
| EListComp {value; region} ->
    pp_loc_node state "EListComp" region;
    if   value.elements = None
    then pp_node (state#pad 1 0) "<nil>"
    else pp_injection state pp_expr value

and pp_string_expr state = function
  Cat e      -> pp_op2 state "Cat" e
| String e   -> pp_node   state "String";
               pp_string (state#pad 1 0) e
| Verbatim e -> pp_node   state "Verbatim";
               pp_string (state#pad 1 0) e

and pp_arith_expr state = function
  Add   e -> pp_op2 state "Add" e
| Sub   e -> pp_op2 state "Sub" e
| Mult  e -> pp_op2 state "Mult" e
| Div   e -> pp_op2 state "Div" e
| Mod   e -> pp_op2 state "Mod" e
| Neg   e -> pp_op1 state "Neg" e
| Int   e -> pp_node state "Int";
            pp_int  state e
| Nat   e -> pp_node state "Nat";
            pp_int  state e
| Mutez e -> pp_node state "Mutez";
            pp_int  state e

and pp_e_logic state = function
  BoolExpr e ->
    pp_node state "BoolExpr";
    pp_bool_expr (state#pad 1 0) e
| CompExpr e ->
    pp_node state "CompExpr";
    pp_comp_expr (state#pad 1 0) e

and pp_bool_expr state = function
  Or    e -> pp_op2      state "Or"    e
| And   e -> pp_op2      state "And"   e
| Not   e -> pp_op1      state "Not"   e
| False e -> pp_loc_node state "False" e
| True  e -> pp_loc_node state "True"  e

and pp_comp_expr state = function
  Lt    e -> pp_op2 state "Lt"    e
| Leq   e -> pp_op2 state "Leq"   e
| Gt    e -> pp_op2 state "Gt"    e
| Geq   e -> pp_op2 state "Geq"   e
| Equal e -> pp_op2 state "Equal" e
| Neq   e -> pp_op2 state "Neq"   e

and pp_op1 state node {value; region} =
  pp_loc_node state node region;
  pp_expr     (state#pad 1 0) value.arg;

and pp_op2 state node {value; region} =
  pp_loc_node state node region;
  pp_expr     (state#pad 2 0) value.arg1;
  pp_expr     (state#pad 2 1) value.arg2

and pp_annotated state annot =
  let expr, _, t_expr = annot.inside in
  pp_expr      (state#pad 2 0) expr;
  pp_type_expr (state#pad 2 1) t_expr

and pp_cond_expr state (cond: cond_expr) =
  let arity = if cond.ifnot = None then 2 else 3 in
  let () =
    let state = state#pad arity 0 in
    pp_node state "<condition>";
    pp_expr (state#pad 1 0) cond.test in
  let () =
    let state = state#pad arity 1 in
    pp_node state "<true>";
    pp_expr (state#pad 1 0) cond.ifso in
  let () =
    match cond.ifnot with
      Some (_, ifnot) ->
        let state = state#pad arity 2 in
        pp_node state "<false>";
        pp_expr (state#pad 1 0) ifnot
    | None -> ()
  in ()

and pp_case :
  'a.state -> (state -> 'a -> unit) -> 'a case -> unit =
  fun state print case ->
    let clauses = Utils.nsepseq_to_list case.cases.value in
    let clauses = List.map (fun x -> x.value) clauses in
    let arity  = List.length clauses + 1 in
    let apply len rank =
      pp_case_clause (state#pad len (rank+1)) print
    in pp_expr (state#pad arity 0) case.expr;
       List.iteri (apply arity) clauses

and pp_case_clause :
  'a.state -> (state -> 'a -> unit) -> 'a case_clause -> unit =
  fun state print clause ->
    pp_node    state "<clause>";
    pp_pattern (state#pad 2 0) clause.pattern;
    print      (state#pad 2 1) clause.rhs

and pp_type_expr state = function
  TProd {value; region} ->
    pp_loc_node state "TProd" region;
    pp_cartesian state value
| TSum {value; region} ->
    pp_loc_node state "TSum" region;
    pp_sum_type state value
| TRecord {value; region} ->
    pp_loc_node    state "TRecord" region;
    pp_record_type state value
| TApp {value=name,tuple; region} ->
    pp_loc_node   state "TApp" region;
    pp_ident      (state#pad 2 0) name;
    pp_type_tuple (state#pad 2 1) tuple
| TFun {value; region} ->
    pp_loc_node state "TFun" region;
    let apply len rank =
      pp_type_expr (state#pad len rank) in
    let domain, _, range = value in
    List.iteri (apply 2) [domain; range]
| TPar {value={inside;_}; region} ->
    pp_loc_node  state "TPar" region;
    pp_type_expr (state#pad 1 0) inside
| TVar v ->
    pp_node  state "TVar";
    pp_ident (state#pad 1 0) v
| TWild wild ->
    pp_node  state "TWild";
    pp_loc_node state "TWild" wild
| TString s ->
    pp_node   state "TString";
    pp_string (state#pad 1 0) s
| TInt s ->
    pp_node   state "TInt";
    pp_int (state#pad 1 0) s
| TModA {value; region} ->
    pp_loc_node state "TModA" region;
    pp_module_access state pp_type_expr value

and pp_sum_type state {variants; attributes; _} =
  let variants = Utils.nsepseq_to_list variants in
  let arity    = List.length variants in
  let arity    = if attributes = [] then arity else arity+1 in
  let apply arity rank variant =
    let state = state#pad arity rank in
    pp_variant state variant.value in
  let () = List.iteri (apply arity) variants in
  if attributes <> [] then
    let state = state#pad arity (arity-1)
    in pp_attributes state attributes

and pp_type_tuple state {value; _} =
  let components     = Utils.nsepseq_to_list value.inside in
  let apply len rank = pp_type_expr (state#pad len rank)
  in List.iteri (List.length components |> apply) components

and pp_field_decl state {value; _} =
  let arity = if value.attributes = [] then 1 else 2 in
  pp_ident     state value.field_name;
  pp_type_expr (state#pad arity 0) value.field_type;
  if value.attributes <> [] then
    pp_attributes (state#pad arity 1) value.attributes

and pp_cartesian state t_exprs =
  let t_exprs        = Utils.nsepseq_to_list t_exprs in
  let arity          = List.length t_exprs in
  let apply len rank = pp_type_expr (state#pad len rank)
  in List.iteri (apply arity) t_exprs

and pp_variant state {ctor; arg; attributes=attr} =
  let arity = if attr = [] then 0 else 1 in
  let arity = if arg = None then arity else arity + 1 in
  let rank  = 0 in
  let () = pp_ident state ctor in
  let rank =
    match arg with
      None -> rank
    | Some (_, c) ->
        pp_type_expr (state#pad arity rank) c; rank+1 in
  if attr <> [] then
    pp_attributes (state#pad arity rank) attr
