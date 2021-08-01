(* This module defines and exports the type [t] of conditional
   expressions of preprocessing directives. *)

type ast =
  Or    of ast * ast
| And   of ast * ast
| Eq    of ast * ast
| Neq   of ast * ast
| Not   of ast
| True
| False
| Ident of string

type t = ast

(* Environments *)

module Env = Set.Make (String)

let rec eval env = function
   Or (e1,e2) -> eval env e1 || eval env e2
| And (e1,e2) -> eval env e1 && eval env e2
|  Eq (e1,e2) -> eval env e1 = eval env e2
| Neq (e1,e2) -> eval env e1 != eval env e2
|       Not e -> not (eval env e)
|        True -> true
|       False -> false
|    Ident id -> Env.mem id env
