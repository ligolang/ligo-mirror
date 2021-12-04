module Errors = Errors
module Helpers = Helpers

let expression_obj ~raise = Obj_ligo.check_obj_ligo ~raise

let monomorphise_expression e =
  let _, e = Monomorphisation.mono_polymorphic_expression [] Monomorphisation.empty_data e in
  e
