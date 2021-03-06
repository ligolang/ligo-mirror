module Var = Simple_utils.Var
open Test_helpers
open Ast_imperative


let file = "./contracts/pledge.ligo"
let mfile = "./contracts/pledge.mligo"
let refile = "./contracts/pledge.religo"


let compile_main ~raise ~add_warning f () =
  Test_helpers.compile_main ~raise ~add_warning f ()

let (oracle_addr , oracle_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (test_environment ()).identities 0 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let (stranger_addr , stranger_contract) =
  let open Proto_alpha_utils.Memory_proto_alpha in
  let id = List.nth_exn (test_environment ()).identities 1 in
  let kt = id.implicit_contract in
  Protocol.Alpha_context.Contract.to_b58check kt , kt

let empty_op_list =
  (e_typed_list [] (t_operation ()))
let empty_message = e_lambda_ez (ValueVar.of_input_var "arguments")
  ~ascr:(t_unit ()) (Some (t_list (t_operation ())))
  empty_op_list


let pledge ~raise ~add_warning f () =
  let program = get_program  ~raise ~add_warning f () in
  let storage = e_address oracle_addr in
  let parameter = e_unit () in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options
                  ~env:(test_environment ())
                  ~sender:oracle_contract
                  ~amount:(Memory_proto_alpha.Protocol.Alpha_context.Tez.one) ())
  in
  expect_eq ~raise ~add_warning ~options program "donate"
    (e_pair parameter storage)
    (e_pair (e_list []) storage)

let distribute ~raise ~add_warning f () =
  let program = get_program ~raise ~add_warning f () in
  let storage = e_address oracle_addr in
  let parameter =  empty_message in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options
                  ~env:(test_environment ())
                  ~sender:oracle_contract ())
  in
  expect_eq ~raise ~add_warning ~options program "distribute"
    (e_pair parameter storage)
    (e_pair (e_list []) storage)

let distribute_unauthorized ~raise ~add_warning f () =
  let program = get_program ~raise ~add_warning f () in
  let storage = e_address oracle_addr in
  let parameter =  empty_message in
  let options = Proto_alpha_utils.Memory_proto_alpha.(make_options
                  ~env:(test_environment ())
                  ~sender:stranger_contract ())
  in
  expect_string_failwith ~raise ~add_warning ~options program "distribute"
    (e_pair parameter storage)
    "You're not the oracle for this distribution."

let main = test_suite "Pledge & Distribute" [
    test_w "donate"                    (pledge                  file) ;
    test_w "distribute"                (distribute              file) ;
    test_w "distribute (unauthorized)" (distribute_unauthorized file) ;
    test_w "donate"                    (pledge                  mfile) ;
    test_w "distribute"                (distribute              mfile) ;
    test_w "distribute (unauthorized)" (distribute_unauthorized mfile) ;
    test_w "donate"                    (pledge                  refile) ;
    test_w "distribute"                (distribute              refile) ;
    test_w "distribute (unauthorized)" (distribute_unauthorized refile) ;
]
