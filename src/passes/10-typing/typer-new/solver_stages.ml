open Trace

module Core = Typesystem.Core
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet
module UF = UnionFind.Poly2
open Solver_types

module M(Plugins : Plugins)(Solver : sig
    type indexers_plugins_states = Plugins.Indexers.Indexers_plugins_fields(PerPluginState).flds
    type nonrec typer_state = indexers_plugins_states Solver_types.typer_state
    module Worklist_and_pending : module type of Worklist_and_pending.M(struct type nonrec indexers_plugins_states = indexers_plugins_states end)
  end) = struct

  open Solver
  open Worklist_and_pending

  let filter_already_added ((state : typer_state), type_constraint) =
    Format.printf "in filter_aleady_added\n%!";
    if PolySet.mem type_constraint state.added_constraints
    then ok (state, Worklist.empty)
    else ok ({ state with added_constraints = PolySet.add type_constraint state.added_constraints},
             { Worklist.empty with pending_filtered_not_already_added_constraints = Pending.singleton type_constraint })

  let simplify_constraint (state, new_constraint) =
    Format.printf "in simplify_consraint\n%!";
    ok (state, { Worklist.empty with pending_type_constraint_simpl = Pending.of_list (Simplifier.type_constraint_simpl new_constraint) })

  let split_aliases (state, new_constraint) =
    Format.printf "in split_aliases\n%!";
    match new_constraint with
      Ast_typed.Types.SC_Alias c_alias ->
      ok (state, { Worklist.empty with pending_c_alias = Pending.singleton c_alias })
    | non_alias ->
      ok (state, { Worklist.empty with pending_non_alias = Pending.singleton non_alias })

end
