let foo (_u : unit) : address = failwith "foo" (* Tezos.address (Tezos.self "%default" : unit contract)*)

let main (_ps: unit * address): (operation list * address) =
  let _dummy = foo () in (* force not to inline foo *)
  ( ([] : operation list) , foo ())
