module Definitions = struct
  module Def_map = Map.Make( struct type t = string let compare = String.compare end)

  type type_case =
    | Core of Ast_core.type_expression
    | Resolved of Ast_typed.type_expression
    | Unresolved

  type vdef = {
    name : string ;
    range : Location.t ;
    body_range : Location.t ;
    t : type_case ;
    references : (Location.t list) option
  }

  type tdef = {
    name : string ;
    range : Location.t ;
    body_range : Location.t ;
    content : Ast_core.type_expression ;
  }
  
  type def = Variable of vdef | Type of tdef
  type def_map = def Def_map.t

  let merge_defs a b = Def_map.union (fun _ a _  -> Some a) a b

  let get_def_name = function
    | Variable d -> d.name
    | Type d -> d.name
  
  let get_range = function
    | Type t -> t.range
    | Variable v -> v.range

  let make_v_def : string -> type_case -> Location.t -> Location.t -> def =
    fun name t range body_range ->
      Variable { name ; range ; body_range ; t ; references = None }

  let make_t_def : string -> Ast_core.declaration Location.wrap -> Ast_core.type_expression -> def =
    fun name decl te ->
      Type { name ; range = decl.location ; body_range = te.location ; content = te }

end

include Definitions

type scope = { range : Location.t ; env : def_map }
type scopes = scope list

let add_scope (range,env) scopes = { range ; env } :: scopes


module Bindings_map = Map.Make ( struct type t = Ast_typed.expression_variable let compare = Ast_typed.Compare.expression_variable end )
type bindings_map = Ast_typed.type_expression Bindings_map.t