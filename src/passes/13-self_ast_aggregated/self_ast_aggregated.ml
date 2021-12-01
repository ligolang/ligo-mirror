let monomorphise_expression e =
  let _, e = Monomorphisation.mono_polymorphic_expression [] Monomorphisation.empty_data e in
  e
