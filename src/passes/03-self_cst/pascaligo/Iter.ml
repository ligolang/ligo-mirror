(* Left-preorder iteration over the PascaLIGO CST *)

(* Vendor dependencies *)

module Utils  = Simple_utils.Utils
module Region = Simple_utils.Region

(* Local dependencies *)

module CST = Cst_pascaligo.CST
open! CST

(* Utilities *)

let fold_right = Stdlib.List.fold_right

type 'a reg = 'a Region.reg
(*type 'payload wrap = 'payload Wrap.t*)

let (<@) f g x = f (g x)

let swap f x y = f y x

module Forest =
  struct
    let identity x = x

    let of_option f opt y =
      match opt with
        Some x -> f x y
      | None   -> y

    let of_par f (p : _ par reg) = f p.value.inside

    let of_nsepseq f = fold_right f <@ Utils.nsepseq_to_list

    let of_tuple node = of_par @@ of_nsepseq node

    let of_compound f (node : 'a compound reg) =
      fold_right f @@ Utils.sepseq_to_list node.value.elements

    (* CST nodes of interest for the accumulator *)

    type tree = [
      `Expr        of CST.expr
    | `Statement   of CST.statement
    | `Type_expr   of CST.type_expr
    | `Declaration of CST.declaration
    ]

    type t = tree list

    type forest = t

    (* The functions to apply to the nodes of interest *)

    type 'a iterated = {
      expr        : 'a -> t -> CST.expr        -> 'a * t;
      statement   : 'a -> t -> CST.statement   -> 'a * t;
      type_expr   : 'a -> t -> CST.type_expr   -> 'a * t;
      declaration : 'a -> t -> CST.declaration -> 'a * t
    }

    (* Iterating on a forest *)

    let rec fold (iter : 'a iterated) = function
      acc, [] -> acc
    | acc, `Expr e :: forest ->
        fold iter (iter.expr acc forest e)
    | acc, `Statement s :: forest ->
        fold iter (iter.statement acc forest s)
    | acc, `Type_expr t :: forest ->
        fold iter (iter.type_expr acc forest t)
    | acc, `Declaration d :: forest ->
        fold iter (iter.declaration acc forest d)

    (* CONCRETE SYNTAX TREE (CST) *)

    (* Extracting the greatest sub-forest from a tree *)

    let rec of_cst (node : CST.t) =
      of_declarations node.decl

    (* DECLARATIONS (top-level) *)

    and of_declarations (node : CST.declarations) =
      fold_right of_declaration @@ Utils.nseq_to_list node

    and of_declaration (node : CST.declaration) =
      Stdlib.List.cons (`Declaration node)
    (* TODO: Client-side
       match node with
         D_Const     d -> of_D_Const  forest d
       | D_Directive _ -> forest
       | D_Fun       d -> of_D_Fun    forest d
       | D_Module    d -> of_D_Module forest d
       | D_ModAlias  _ -> forest
       | D_Type      d -> of_D_Type   forest d
     *)

    (* Constant declaration *)

    and of_D_Const (node : CST.const_decl reg) =
      let node = node.value
      in of_pattern node.pattern
      <@ of_option of_type_annotation node.const_type
      <@ of_expr node.init

    and of_type_annotation (node : type_annotation) =
      of_type_expr (snd node)

    (* Function declaration *)

    and of_D_Fun (node : fun_decl reg) =
      let node = node.value
      in of_parameters node.parameters
      <@ of_option of_type_annotation node.ret_type
      <@ of_expr node.return

    and of_parameters (node : parameters) =
      of_par (of_nsepseq of_param_decl) node

    and of_param_decl (node : param_decl reg) =
      of_option of_type_annotation node.value.param_type

    (* Module declaration (structures) *)

    and of_D_Module (node : module_decl reg) =
      of_declarations node.value.declarations

    (* Type declaration *)

    and of_D_Type (node : type_decl reg) =
      of_type_expr node.value.type_expr

    (* TYPE EXPRESSIONS *)

    and of_type_expr (node : type_expr) =
      Stdlib.List.cons (`Type_expr node)
    (* TODO: Client-side
      match node with
        T_App     t -> of_T_App     t
      | T_Cart    t -> of_T_Cart    t
      | T_Fun     t -> of_T_Fun     t
      | T_Int     t -> of_T_Int     t
      | T_ModPath t -> of_T_ModPath t
      | T_Par     t -> of_T_Par     t
      | T_Record  t -> of_T_Record  t
      | T_String  t -> of_T_String  t
      | T_Sum     t -> of_T_Sum     t
      | T_Var     t -> of_T_Var     t
    *)

    (* The following functions are called by the client (field
       [type_expr] of type ['a iterated]. *)

    (* Application of type constructors *)

    and of_T_App (node : (type_expr * type_tuple) reg) =
      let ctor, args = node.value
      in of_type_expr ctor
      <@ of_type_tuple args

    and of_type_tuple (node : type_tuple) =
      of_par (of_nsepseq of_type_expr) node

    (* Cartesian type *)

    and of_T_Cart (node : cartesian) =
      let head, sep, tail = node.value in
      let seq = Utils.nsepseq_cons head sep tail
      in of_nsepseq of_type_expr seq

    (* Functional type *)

    and of_T_Fun (node : (type_expr * arrow * type_expr) reg) =
      let domain, _, range = node.value
      in of_type_expr domain
      <@ of_type_expr range

    (* Integer type *)

    and of_T_Int (node : (lexeme * Z.t) wrap) =
      ignore node; identity

    (* Qualified type expression *)

    and of_T_ModPath (node : type_expr module_path reg) =
      of_type_expr node.value.field

    (* Parenthesised type *)

    and of_T_Par (node : type_expr par reg) =
      of_par of_type_expr node

    (* Record type *)

    and of_T_Record (node : field_decl reg compound reg) =
      of_compound of_field_decl node

    and of_field_decl (node : field_decl reg) =
      of_option of_type_annotation node.value.field_type

    (* String type *)

    and of_T_String (node : lexeme wrap) = ignore node; identity

    (* Sum type *)

    and of_T_Sum (node : sum_type reg) =
      of_nsepseq of_variant node.value.variants

    and of_variant (node : variant reg) =
      of_option (of_type_expr <@ snd) node.value.args


    (* STATEMENTS *)

    and of_statements (node : statements) =
      fold_right of_statement @@ Utils.nsepseq_to_list node

    and of_statement (node : statement) =
      Stdlib.List.cons (`Statement node)
    (* TODO: Client-side
       match node with
         S_Instr   i -> of_S_Instr   i
       | S_Decl    i -> of_S_Decl    i
       | S_VarDecl i -> of_S_VarDecl i
     *)

    and of_S_Instr (node : instruction) =
      of_instruction node

    and of_S_Decl (node : declaration) =
      of_declaration node

    and of_S_VarDecl (node : var_decl reg) =
      let node = node.value
      in of_pattern node.pattern
      <@ of_option of_type_annotation node.var_type
      <@ of_expr node.init

    (* INSTRUCTIONS *)

    and of_instruction (node : instruction) =
      match node with
        I_Assign i -> of_I_Assign i
      | I_Call   i -> of_I_Call   i
      | I_Case   i -> of_I_Case   i
      | I_Cond   i -> of_I_Cond   i
      | I_For    i -> of_I_For    i
      | I_ForIn  i -> of_I_ForIn  i
      | I_Patch  i -> of_I_Patch  i
      | I_Remove i -> of_I_Remove i
      | I_Skip   i -> of_I_Skip   i
      | I_While  i -> of_I_While  i

    (* Assignment *)

    and of_I_Assign (node : assignment reg) =
      let node = node.value
      in of_expr node.lhs <@ of_expr node.rhs

    (* Procedure call *)

    and of_I_Call (node : call) =
      let lambda, arguments = node.value
      in of_expr lambda <@ of_tuple of_expr arguments

    (* Case *)

    and of_I_Case (node : test_clause case reg) =
      of_case of_test_clause node

    and of_case :
      'a.('a -> forest -> forest) -> 'a case reg -> forest -> forest =
      fun f node ->
        let node = node.value
        in of_expr node.expr
        <@ of_nsepseq (of_case_clause f) node.cases

    and of_case_clause :
      'a.('a -> forest -> forest) -> 'a case_clause reg -> forest -> forest =
      fun f node -> of_pattern node.value.pattern <@ f node.value.rhs

    and of_test_clause (node : test_clause) =
      match node with
        ClauseInstr i -> of_instruction i
      | ClauseBlock b -> of_block b

    (* Blocks *)

    and of_block (node : block reg) =
      of_statements node.value.statements

    (* General conditionals *)

    and of_I_Cond (node : test_clause conditional reg) =
      of_conditional of_test_clause node

    and of_conditional :
      'a.('a -> forest -> forest) -> 'a conditional reg -> forest -> forest =
      fun f node ->
        let node = node.value
        in of_expr node.test
        <@ f node.if_so
        <@ of_option (f <@ snd) node.if_not

    (* Iteration over integer intervals *)

    and of_I_For (node : for_int reg) =
      let node = node.value
      in of_expr node.init
      <@ of_expr node.bound
      <@ of_option (of_expr <@ snd) node.step
      <@ of_block node.block

    (* Iteration over maps and sets *)

    and of_I_ForIn (node : for_in reg) =
      let node = node.value
      in of_expr node.collection
      <@ of_block node.block

    (* Patches for maps, records, and sets. *)

    and of_I_Patch (node : patch reg) =
      let node = node.value
      in of_expr node.collection
      <@ of_expr node.patch

    (* Removal from sets and maps *)

    and of_I_Remove (node : removal reg) =
      let node = node.value
      in of_expr node.item
      <@ of_expr node.collection

    (* Skip instruction *)

    and of_I_Skip (node : kwd_skip) = ignore node; identity

    (* General loop *)

    and of_I_While (node : while_loop reg) =
      let node = node.value
      in of_expr node.cond
      <@ of_block node.block


    (* PATTERNS *)

    and of_pattern (node : pattern) =
      match node with
        P_App     p -> of_P_App     p
      | P_Bytes   p -> of_P_Bytes   p
      | P_Cons    p -> of_P_Cons    p
      | P_Ctor    p -> of_P_Ctor    p
      | P_Int     p -> of_P_Int     p
      | P_List    p -> of_P_List    p
      | P_ModPath p -> of_P_ModPath p
      | P_Mutez   p -> of_P_Mutez   p
      | P_Nat     p -> of_P_Nat     p
      | P_Nil     p -> of_P_Nil     p
      | P_Par     p -> of_P_Par     p
      | P_Record  p -> of_P_Record  p
      | P_String  p -> of_P_String  p
      | P_Tuple   p -> of_P_Tuple   p
      | P_Typed   p -> of_P_Typed   p
      | P_Var     p -> of_P_Var     p

    (* Application to a constructor in a pattern *)

    and of_P_App (node : (pattern * pattern tuple option) reg) =
      let ctor, tuple_opt = node.value
      in of_pattern ctor
      <@ of_option (of_tuple of_pattern) tuple_opt

    (* Bytes in patterns *)

    and of_P_Bytes (node : (lexeme * Hex.t) wrap) =
      ignore node; identity

    (* Consing in patterns *)

    and of_P_Cons (node : (pattern * sharp * pattern) reg) =
      let head, _, tail = node.value
      in of_pattern head <@ of_pattern tail

    (* Data constructor in patterns *)

    and of_P_Ctor (node : ctor) = ignore node; identity

    (* Integers in patterns *)

    and of_P_Int (node : (lexeme * Z.t) wrap) =
      ignore node; identity

    (* List patterns *)

    and of_P_List (node : pattern compound reg) =
      of_compound of_pattern node

    (* Module paths in patterns *)

    and of_P_ModPath (node : pattern module_path reg) =
      of_pattern node.value.field

    (* Mutez in patterns *)

    and of_P_Mutez (node : (lexeme * Int64.t) wrap) =
      ignore node; identity

    (* Natural numbers in patterns *)

    and of_P_Nat (node : (lexeme * Z.t) wrap) =
      ignore node; identity

    (* The empty list as a pattern *)

    and of_P_Nil (node : kwd_nil) =
      ignore node; identity

    (* Parenthesised patterns *)

    and of_P_Par (node : pattern par reg) =
      of_par of_pattern node

    (* Record patterns *)

    and of_P_Record (node : record_pattern) =
      of_compound of_field_pattern node

    and of_field_pattern (node : field_pattern reg) =
      of_field ~of_lhs:(fun _ -> identity) ~of_rhs:of_pattern node.value

    and of_field :
      'lhs 'rhs.of_lhs:('lhs -> forest -> forest) ->
                of_rhs:('rhs -> forest -> forest) ->
                ('lhs, 'rhs) field ->
                forest ->
                forest =
      fun ~of_lhs ~of_rhs ->
        function
          Punned   field -> of_lhs field.pun
        | Complete field -> of_lhs field.field_lhs <@ of_rhs field.field_rhs

    (* String as pattern *)

    and of_P_String (node : lexeme wrap) = ignore node; identity

    (* Pattern tuple *)

    and of_P_Tuple (node : pattern tuple) =
      of_tuple of_pattern node

    (* Typed patterns *)

    and of_P_Typed (node : typed_pattern reg) =
      of_type_annotation node.value.type_annot

    (* Pattern variable *)

    and of_P_Var (node : variable) = ignore node; identity

    (* EXPRESSIONS *)

    and of_expr (node : expr) =
      Stdlib.List.cons (`Expr node)
(* TODO: Client-side
      E_Add       e -> of_E_Add e
    | E_App       e -> of_E_App e
    | E_And       e -> of E_And e
    | E_BigMap    e -> of_E_BigMap e
    | E_Block     e -> of_E_Block e
    | E_Bytes     e -> of_E_Bytes e
    | E_Call      e -> of_E_Call e
    | E_Case      e -> of_E_Case e
    | E_Cat       e -> of_E_Cat e
    | E_CodeInj   e -> of_E_CodeInj e
    | E_Ctor      e -> of_E_Ctor e
    | E_Equal     e -> of_E_Equal e
    | E_Cond      e -> of_E_Cond e
    | E_Cons      e -> of_E_Cons e
    | E_Div       e -> of_E_Div e
    | E_Fun       e -> of_E_Fun e
    | E_Geq       e -> of_E_Geq e
    | E_Gt        e -> of_E_Gt e
    | E_Int       e -> of_E_Int e
    | E_Leq       e -> of_E_Leq e
    | E_List      e -> of_E_List e
    | E_Lt        e -> of_E_Lt e
    | E_Map       e -> of_E_Map e
    | E_MapLookup e -> of_E_MapLookup e
    | E_Mod       e -> of_E_Mod e
    | E_ModPath   e -> of_E_ModPath e
    | E_Mult      e -> of_E_Mult e
    | E_Mutez     e -> of_E_Mutez e
    | E_Nat       e -> of_E_Nat e
    | E_Neg       e -> of_E_Neg e
    | E_Nil       e -> of_E_Nil e
    | E_Neq       e -> of_E_Neq e
    | E_Not       e -> of_E_Not e
    | E_Or        e -> of_E_Or e
    | E_Par       e -> of_E_Par e
    | E_Record    e -> of_E_Record e
    | E_Set       e ->
    | E_SetMem    e ->
    | E_String    e ->
    | E_Sub       e ->
    | E_Tuple     e ->
    | E_Typed     e ->
    | E_Update    e ->
    | E_Verbatim  e ->
    | E_Var       e ->
    | E_Proj      e ->
  *)

  end
