open Helpers
open Simple_utils.Trace
open Ast_typed
module Context = Checking.Context

let rec declaration_to_expression ~raise : Context.t -> declaration_loc list -> (string * expression) list = fun env decl ->
  let self = declaration_to_expression ~raise in
  match decl with
    [] -> []
  | (hd: declaration_loc) :: tl -> 
    match hd.wrap_content with
      Declaration_constant {name=_;binder;expr;attr=_} -> 
      let _,expr = fold_map_expression (peephole_expression ~raise) env expr in
      let env = Context.add_value binder expr.type_expression env in
      let binder = Var.to_name binder.wrap_content in
      (binder,expr) :: self env tl
    | Declaration_type _ -> self env tl
    | Declaration_module {module_binder;module_;module_attr=_} ->
      let expr = module_to_record ~raise env module_ in
      let env = Context.add_value 
        (Location.wrap @@ Var.of_name module_binder) expr.type_expression env in
      (module_binder,expr) :: self env tl
    | Module_alias {alias;binders} ->
      let let_binder = Location.wrap @@ Var.of_name alias in
      let (init,nexts) = binders in
      (match Context.get_value env (Location.wrap @@ Var.of_name init) with None -> raise.raise @@ Errors.corner_case "The module shouldn't type"
        | Some (record_type) ->
          let rhs = List.fold ~f:(fun record path -> 
            let record_type = Option.value_exn (get_t_record record.type_expression) in
            let type_expression = match (LMap.find_opt path record_type.content) with None -> raise.raise @@ Errors.corner_case "Module shouldn't type"
              | Some (r) -> r.associated_type in
            {expression_content = E_record_accessor {record;path}; type_expression;location=hd.location}) 
            ~init:(e_a_variable (Location.wrap @@ Var.of_name init) @@ record_type) @@ List.map ~f:(fun x -> Label x) nexts in
          let env = Context.add_value let_binder rhs.type_expression env in
          (alias, rhs) :: self env tl
      )
    
and module_to_record ~raise : Context.t -> module_fully_typed -> expression = fun e m ->
  let Module_Fully_Typed lst = m in
  let lst = declaration_to_expression ~raise e lst in
  let f = fun expr (binder,ex) ->
    let var = Location.wrap @@ Var.of_name binder in
    let attr = {inline=true; no_mutation=false;view=false;public=true} in
    let expr = fun e -> expr @@ 
      e_a_let_in var ex e attr
    in
    (expr, (Label binder, e_a_variable var ex.type_expression))
  in
  let expr, record = List.fold_map ~f ~init:(fun e -> e) lst in
  expr @@ e_a_record ~layout:L_comb @@ LMap.of_list record

and peephole_expression ~raise : Context.t -> expression -> bool * Context.t * expression = fun e expr ->
  match expr.expression_content with
    E_mod_in {module_binder; rhs; let_result} ->
    let let_binder = Location.wrap @@ Var.of_name module_binder in
    let rhs = module_to_record ~raise e rhs in
    let e = Context.add_value let_binder rhs.type_expression e in
    true,e,{ expr with expression_content=E_let_in {let_binder; rhs;let_result; attr={inline=true;no_mutation=false;view=false;public=false}}}
  | E_mod_alias {alias;binders;result} ->
    let let_binder = Location.wrap @@ Var.of_name alias in
    let (init,nexts) = binders in
    (match Context.get_value e (Location.wrap @@ Var.of_name init) with None -> raise.raise @@ Errors.corner_case "The module shouldn't type"
      | Some (record_type) ->
        let rhs = List.fold ~f:(fun record path -> 
          let record_type = Option.value_exn (get_t_record record.type_expression) in
          let type_expression = match (LMap.find_opt path record_type.content) with None -> raise.raise @@ Errors.corner_case "Module shouldn't type"
            | Some (r) -> r.associated_type in
          { expr with expression_content = E_record_accessor {record;path}; type_expression}) 
          ~init:(e_a_variable (Location.wrap @@ Var.of_name init) @@ record_type) @@ List.map ~f:(fun x -> Label x) nexts in
        let attr = { inline = true;no_mutation=false;view=false;public=false } in
        let e = Context.add_value let_binder rhs.type_expression e in
        true,e,{expr with expression_content=E_let_in {let_binder;rhs;let_result=result;attr}}
    )
  | E_module_accessor {module_name; element} ->
    let module_var = Location.wrap @@ Var.of_name module_name in
    (match Context.get_value e module_var with None -> raise.raise @@ Errors.corner_case "The module shouldn't type"
      | Some (record_type) ->
      let module_var = e_a_variable module_var record_type in
      let rec aux (element : expression) = 
        (match element.expression_content with
          | E_module_accessor {module_name;element} -> (Label module_name) :: aux element
          | E_variable var -> [Label (Var.to_name @@ Location.unwrap var)]
          | E_record_accessor {record; path} -> aux record @ [path]
          | _ -> raise.raise @@ Errors.corner_case 
            @@ Format.asprintf "The parser shouldn't allowed this construct : %a" Ast_typed.PP.expression element
        )
      in
      let acces_list = aux element in
      let expr = List.fold ~f:(fun record path ->
        let record_type = Option.value_exn (get_t_record record.type_expression) in
        let type_expression = match (LMap.find_opt path record_type.content) with None -> raise.raise @@ Errors.corner_case "Module shouldn't type"
          | Some (r) -> r.associated_type in
        { expr with expression_content = E_record_accessor {record;path}; type_expression }) 
        ~init:module_var acces_list in
      true,e,expr
    )
  | E_type_inst _ -> raise.raise @@ Errors.corner_case "Monomorphisation should run before Module Morphing"
  | _ -> true,e,expr

let peephole_declaration ~raise : Context.t -> declaration_loc -> Context.t * declaration_loc = fun e m ->
  match m.wrap_content with
    Declaration_module {module_binder;module_;module_attr} ->
    let binder = Location.wrap @@ Var.of_name @@ module_binder in
    let expr = module_to_record ~raise e module_ in
    let attr = {inline=true; no_mutation=false;view=false;public=module_attr.public} in
    let e = Context.add_value binder expr.type_expression e in
    e,{ m with wrap_content=Declaration_constant {name=None;binder;expr;attr}}
  | Module_alias {alias;binders} ->
    let (init,nexts) = binders in 
    let binder = Location.wrap @@ Var.of_name @@ alias in
    (match Context.get_value e (Location.wrap @@ Var.of_name init) with None -> raise.raise @@ Errors.corner_case "The module shouldn't type"
    | Some (record_type) ->
      let expr = 
        let record_expr = e_a_variable (Location.wrap @@ Var.of_name init) (record_type) in
        List.fold ~f:(fun record path ->
          let record_type = Option.value_exn (get_t_record record.type_expression) in
          let type_expression = match (LMap.find_opt path record_type.content) with None -> raise.raise @@ Errors.corner_case "Module shouldn't type"
            | Some (r) -> r.associated_type in
          {record with expression_content = E_record_accessor {record;path}; type_expression}
          ) ~init:record_expr @@ List.map ~f:(fun s -> Label s) nexts 
      in
      let e = Context.add_value binder expr.type_expression e in
      let attr = {inline=true; no_mutation=false;view=false;public=true} in
      e,{ m with wrap_content = Declaration_constant {name=None;binder;expr;attr}}
    )
  | Declaration_constant {name;binder;expr;attr} ->
    let _,expr = fold_map_expression (peephole_expression ~raise) e expr in
    let e = Context.add_value binder expr.type_expression e in
    e,{ m with wrap_content = Declaration_constant {name;binder;expr;attr}}

  | Declaration_type _ -> e,m

let peephole_module ~raise : Context.t -> module_fully_typed -> Context.t * module_fully_typed = fun e m ->
  let Module_Fully_Typed m = m in
  let e,m = List.fold_map ~f:(peephole_declaration ~raise) ~init:e m in
  e,Module_Fully_Typed m

let peephole_program ~raise : Environment.t -> module_fully_typed -> module_fully_typed = fun e m -> snd @@ peephole_module ~raise (Context.init ~with_stdlib:e ()) m
