open Core
module Types = struct 
  open Ast_typed 
  module EVar = struct
    type t = expression_variable [@@deriving compare,sexp_of]
  end
  module VComp = struct include EVar include Comparator.Make(EVar) end (* TODO : make type structure*) 
  module TVar = struct
    type t = type_variable [@@deriving compare,sexp_of]
  end
  module TComp = struct include TVar include Comparator.Make(TVar) end (* TODO : make type structure*)
  module MVar = struct
    type t = module_variable [@@deriving compare,sexp_of]
  end
  module MComp = struct include MVar include Comparator.Make(MVar) end (* TODO : make type structure*)

  type values  = (expression_variable, type_expression, VComp.comparator_witness) Map.t
  type types   = (type_variable      , type_expression, TComp.comparator_witness) Map.t
  type modules = (module_variable    , context,         MComp.comparator_witness) Map.t
  and  context = { (* TODO : move to sets, requires new architecture *)
    values  : values  ;
    types   : types   ;
    modules : modules ;
  }
end

type t = Types.context

module PP = struct
  open Format
  open Ast_typed.PP
  open Simple_utils.PP_helpers
  open Types

  let list_sep_scope x = list_sep x (const " | ")
  let value_binding ppf (ev,te) = 
    fprintf ppf "%a => %a" expression_variable ev type_expression te
  let type_binding ppf (type_var,type_) =
    fprintf ppf "%a => %a" type_variable type_var type_expression type_

  let rec module_binding ppf (mod_var,type_) =
    fprintf ppf "%a => %a" module_variable mod_var context type_

  and context ppf {values;types;modules} =
    fprintf ppf "{[ %a; @; %a; %a; ]}"
      (list_sep_scope value_binding ) (Map.to_alist values )
      (list_sep_scope type_binding  ) (Map.to_alist types  )
      (list_sep_scope module_binding) (Map.to_alist modules)

end

let empty : t = { values = Map.empty (module Types.VComp); types = Map.empty (module Types.TComp) ; modules = Map.empty (module Types.MComp) }

let get_values : t -> Types.values = fun { values ; types=_ ; modules=_ } -> values
(* TODO: generate *)
let get_types  : t -> Types.types  = fun { values=_ ; types ; modules=_ } -> types
(* TODO: generate *)
let get_modules : t -> Types.modules = fun { values=_ ; types=_ ; modules } -> modules


(* TODO: generate : these are now messy, clean them up. *)
let add_value : Ast_typed.expression_variable -> Ast_typed.type_expression -> t -> t = fun ev te e -> 
  let values = Map.update e.values ~f:(fun _ -> te) ev in
  {e with values}

let add_type : Ast_typed.type_variable -> Ast_typed.type_expression -> t -> t = fun tv te e -> 
  let types = Map.update e.types ~f:(fun _ -> te) tv in
  {e with types}
let add_module : Ast_typed.module_variable -> t -> t -> t = fun mv te e -> 
  let modules = Map.update e.modules ~f:(fun _ -> te) mv in
  {e with modules}

let get_value (e:t) = Map.find e.values
let get_type (e:t) = Map.find e.types
let get_module (e:t) = Map.find e.modules

let init ?(with_stdlib:Environment.t option) () = 
  match with_stdlib with None -> empty
  | Some (env) ->
    let rec f c d = match Simple_utils.Location.unwrap d with
      Ast_typed.Declaration_constant {binder;expr;_} -> add_value binder expr.type_expression c
    | Declaration_type {type_binder;type_expr;_}     -> add_type  type_binder type_expr c
    | Declaration_module {module_binder;module_=Ast_typed.Module_Fully_Typed m; module_attr=_}     -> add_module module_binder (List.fold ~f ~init:empty m) c
    | _ -> failwith "gnagnagna"
    in
    List.fold ~f ~init:empty env


type label = Stage_common.Types.label
open Ast_typed.Types


let get_constructor : label -> t -> (type_expression * type_expression) option = fun k x -> (* Left is the constructor, right is the sum type *)
  let rec rec_aux e =
    let aux = fun type_ ->
    match type_.type_content with
    | T_sum m ->
      (match LMap.find_opt k m.content with
          Some {associated_type ; _} -> Some (associated_type , type_)
        | None -> None)
    | _ -> None
    in
    match List.find_map ~f:aux @@ Map.data @@ get_types e with
      Some _ as s -> s
    | None ->
      let modules = get_modules e in
      List.fold_left ~f:(fun res module_ ->
        match res with Some _ as s -> s | None -> rec_aux module_
      ) ~init:None @@ Map.data modules
  in rec_aux x

let get_constructor_parametric : label -> t -> (type_variable list * type_expression * type_expression) option = fun k x -> (* Left is the constructor, right is the sum type *)
  let rec rec_aux e =
    let rec aux av = fun type_ ->
      match type_.type_content with
      | T_sum m ->
         (match LMap.find_opt k m.content with
            Some {associated_type ; _} -> Some (av, associated_type , type_)
          | None -> None)
      | T_abstraction { ty_binder ; kind = _ ; type_ } ->
         aux (Location.unwrap ty_binder :: av) type_
      | _ -> None in
    let aux = aux []in
    match List.find_map ~f:aux @@ Map.data (get_types e) with
      Some _ as s -> s
    | None ->
      let modules = get_modules e in
      List.fold_left ~f:(fun res module_ ->
        match res with Some _ as s -> s | None -> rec_aux module_
      ) ~init:None @@ Map.data modules
  in rec_aux x

let get_record : _ label_map -> t -> (type_variable option * rows) option = fun lmap e ->
  let rec rec_aux e =
    let aux = fun type_ ->
    match type_.type_content with
    | T_record m -> Simple_utils.Option.(
      let lst_kv  = LMap.to_kv_list_rev lmap in
      let lst_kv' = LMap.to_kv_list_rev m.content in
      let m = map ~f:(fun () -> m) @@ Ast_typed.Misc.assert_list_eq
        ( fun (ka,va) (kb,vb) ->
          let Label ka = ka in
          let Label kb = kb in
          let* () = Ast_typed.Misc.assert_eq ka kb in
          Ast_typed.Misc.assert_type_expression_eq (va.associated_type, vb.associated_type)
        ) lst_kv lst_kv' in
      map ~f:(fun m -> (type_.orig_var,m)) @@ m
    )
    | _ -> None
    in
    match List.find_map ~f:aux @@ Map.data (get_types e) with
      Some _ as s -> s
    | None ->
      let modules = get_modules e in
      List.fold_left ~f:(fun res module_ ->
        match res with Some _ as s -> s | None -> rec_aux module_
      ) ~init:None @@ Map.data modules
  in rec_aux e


let get_sum : _ label_map -> t -> rows option = fun lmap e ->
  let rec rec_aux e =
    let aux = fun type_ ->
    match type_.type_content with
    | T_sum m -> Simple_utils.Option.(
      let lst_kv  = LMap.to_kv_list_rev lmap in
      let lst_kv' = LMap.to_kv_list_rev m.content in
      map ~f:(fun () -> m) @@ Ast_typed.Misc.assert_list_eq (
        fun (ka,va) (kb,vb) ->
          let Label ka = ka in
          let Label kb = kb in
          let* () = Ast_typed.Misc.assert_eq ka kb in
          Ast_typed.Misc.assert_type_expression_eq (va.associated_type, vb.associated_type)
      ) lst_kv lst_kv'
    )
    | _ -> None
    in
    match List.find_map ~f:aux @@ Map.data (get_types e) with
      Some _ as s -> s
    | None ->
      let modules = get_modules e in
      List.fold_left ~f:(fun res module_ ->
        match res with Some _ as s -> s | None -> rec_aux module_
      ) ~init:None @@ Map.data modules
  in rec_aux e

