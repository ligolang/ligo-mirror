type michelson_program = unit
type michelson_contract = unit
type test_exec_error_balance_too_low = { contract_too_low : address ; contract_balance : tez ; spend_request : tez }
type test_exec_error = Rejected of michelson_program * address
                     | Balance_too_low of test_exec_error_balance_too_low
                     | Other of string
type test_exec_result = Success of nat | Fail of test_exec_error
type test_baker_policy =
  | By_round of int
  | By_account of address
  | Excluding of address list
module Test = struct
  [@private] let failwith (type a b) = [%Michelson ({|{ FAILWITH }|} : a -> b)]
  type ('a, 'b) typed_address = unit
  type michelson_program = unit
  type test_exec_result = unit
  type mutation = unit
  let to_contract (type p s) (_t : (p, s) typed_address) : p contract = failwith "TEST MODE"
  let originate_from_file (_fn : string) (_e : string) (_v : string list) (_s : michelson_program)  (_t : tez) : address * michelson_contract * int = failwith "TEST MODE"
  let originate (type p s) (_f : p * s -> operation list * s) (_s : s) (_t : tez) : ((p, s) typed_address * michelson_contract * int) = failwith "TEST MODE"
  let set_source (_a : address) : unit = failwith "TEST MODE"
  let set_baker (_a : address) : unit = failwith "TEST MODE"
  let set_baker_policy (_bp : test_baker_policy) : unit = failwith "TEST MODE"
  let transfer (_a : address) (_s : michelson_program) (_t : tez) : test_exec_result = failwith "TEST MODE"
  let transfer_exn (_a : address) (_s : michelson_program) (_t : tez) : nat = failwith "TEST MODE"
  let transfer_to_contract (type p) (_a : p contract) (_s : p) (_t : tez) : test_exec_result = failwith "TEST MODE"
  let transfer_to_contract_exn (type p) (_a : p contract) (_s : p) (_t : tez) : nat = failwith "TEST MODE"
  let get_storage (type a b) (_t : (a, b) typed_address) : b = failwith "TEST MODE"
  let get_storage_of_address (_a : address) : michelson_program = failwith "TEST MODE"
  let get_balance (_a : address) : tez = failwith "TEST MODE"
  let michelson_equal (_m1 : michelson_program) (_m2 : michelson_program) : bool = failwith "TEST MODE"
  let log (type a) (_v : a) : unit = failwith "TEST MODE"
  let reset_state (_n : nat) (_l : tez list) : unit = failwith "TEST MODE"
  let get_voting_power (_kh : key_hash) : nat = failwith "TEST MODE"
  [@thunk] let get_total_voting_power : nat = failwith "TEST MODE"
  let bootstrap_contract (type p s) (_f : p * s -> operation list * s) (_s : s) (_t : tez) : unit = failwith "TEST MODE"
  let nth_bootstrap_contract (_i : nat) : address = failwith "TEST MODE"
  let nth_bootstrap_account (_i : int) : address = failwith "TEST MODE"
  let nth_bootstrap_typed_address (type a b) (_n : nat) : (a, b) typed_address = failwith "TEST MODE"
  let last_originations (_u : unit) : (address, address list) map = failwith "TEST MODE"
  let compile_value (type a) (_v : a) : michelson_program = failwith "TEST MODE"
  let mutate_value (type a) (_n : nat) (_v : a) : (a * mutation) option = failwith "TEST MODE"
  let save_mutation (_s : string) (_m : mutation) : string option = failwith "TEST MODE"
  let mutation_test (type a b) (_v : a) (_f : a -> b) : (b * mutation) option = failwith "TEST MODE"
  let mutation_test_all (type a b) (_v : a) (_f : a -> b) : (b * mutation) list = failwith "TEST MODE"
  let run (type a b) (_f : a -> b) (_v : a) : michelson_program = failwith "TEST MODE"
  let eval (type a) (_v : a) : michelson_program = failwith "TEST MODE"
  let decompile (type a) (_m : michelson_program) : a = failwith "TEST MODE"
  let random (type a) (_u : unit) : a option = failwith "TEST MODE"
  let add_account (_s : string) (_k : key) : unit = failwith "TEST MODE"
  let new_account (_u : unit) : string * key = failwith "TEST MODE"
  let baker_account (_p : string * key) (_o : tez option) : unit = failwith "TEST MODE"
  let bake_until_n_cycle_end (_n : nat) : unit = failwith "TEST MODE"
  let register_delegate (_kh : key_hash) : unit = failwith "TEST MODE"
  let register_constant (_m : michelson_program) : string = failwith "TEST MODE"
  let cast_address (type a b) (_a : address) : (a, b) typed_address = failwith "TEST MODE"
  let to_typed_address (type a b) (_c : a contract) : (a, b) typed_address = failwith "TEST MODE"
  let to_entrypoint (type a b c) (_s : string) (_t : (a, b) typed_address) : c contract = failwith "TEST MODE"
  let set_big_map (type a b) (_i : int) (_m : (a, b) big_map) : unit = failwith "TEST MODE"
  let create_chest (_b : bytes) (_n : nat) : chest * chest_key = failwith "TEST MODE"
  let create_chest_key (_c : chest) (_n : nat) : chest_key = failwith "TEST MODE"
  let constant_to_michelson_program (_s : string) : michelson_program = failwith "TEST MODE"
  let restore_context (_u : unit) : unit = failwith "TEST_POP_CONTEXT"
  let save_context (_u : unit) : unit = failwith "TEST_PUSH_CONTEXT"
  let read_contract_from_file (_fn : string) : michelson_contract = failwith "TEST_READ_CONTRACT_FROM_FILE"
  let compile_contract_from_file  (_fn : string) (_e : string) (_v : string list) : michelson_contract = failwith "TEST_COMPILE_CONTRACT_FROM_FILE"
  let compile_contract (type p s) (_f : p * s -> operation list * s) : michelson_contract = failwith "TEST_COMPILE_CONTRACT"
  let originate_contract (_c : michelson_contract) (_s : michelson_program) (_t : tez) : address = failwith "TEST_ORIGINATE"
  let size (_c : michelson_contract) : int = failwith "TEST_SIZE"
  let sign (_sk : string) (_d : bytes) = failwith "TEST_SIGN"
end