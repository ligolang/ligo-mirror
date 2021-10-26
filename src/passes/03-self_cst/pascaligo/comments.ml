module CST    = Cst.Pascaligo

let peephole_type ~raise ~comments : CST.type_expr -> CST.type_expr = fun t -> 
  ignore raise; ignore comments;
  t

let peephole_expression ~raise ~comments : CST.expr -> CST.expr = fun e ->
  ignore raise; ignore comments;
  e

let peephole_declaration ~raise ~comments : CST.declaration -> CST.declaration = fun s ->
  ignore raise; ignore comments;
  s 

let peephole_statement ~raise ~comments : CST.statement -> CST.statement = fun s ->
  ignore raise; ignore comments;
  s 

let peephole ~raise ~comments : ('err) Helpers.mapper = {
  t = peephole_type ~raise ~comments;
  e = peephole_expression ~raise ~comments;
  d = peephole_declaration ~raise ~comments;
  s = peephole_statement ~raise ~comments
}