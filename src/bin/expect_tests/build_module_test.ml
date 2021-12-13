open Cli_expect

let contract basename =
  "../../test/contracts/build/" ^ basename

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "cycle_A.mligo" ] ;
  [%expect {|
    `-- ../../test/contracts/build/cycle_A.mligo
        `-- ../../test/contracts/build/cycle_B.mligo
            `-- ../../test/contracts/build/cycle_C.mligo
                `-- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "cycle_A.mligo"; "--format" ; "json" ] ;
  [%expect {|
    {
      "root": "../../test/contracts/build/cycle_A.mligo",
      "child": {
        "file": "../../test/contracts/build/cycle_B.mligo",
        "child": {
          "file": "../../test/contracts/build/cycle_C.mligo",
          "child": {
            "file": "../../test/contracts/build/cycle_A.mligo",
            "child": { "file": "../../test/contracts/build/cycle_B.mligo" }
          }
        }
      }
    } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "D.mligo" ] ;
  [%expect {|
    `-- ../../test/contracts/build/D.mligo
        |-- ../../test/contracts/build/C.mligo
        |   |-- ../../test/contracts/build/A.mligo
        |   `-- ../../test/contracts/build/B.mligo
        |       `-- ../../test/contracts/build/A.mligo
        `-- ../../test/contracts/build/E.mligo
            |-- ../../test/contracts/build/F.mligo
            `-- ../../test/contracts/build/G.mligo |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "dependency-graph" ; contract "D.mligo"; "--format" ; "json" ] ;
  [%expect {|
    {
      "root": "../../test/contracts/build/D.mligo",
      "child": {
        "file": "../../test/contracts/build/C.mligo",
        "child": { "file": "../../test/contracts/build/A.mligo" },
        "child": {
          "file": "../../test/contracts/build/B.mligo",
          "child": { "file": "../../test/contracts/build/A.mligo" }
        }
      },
      "child": {
        "file": "../../test/contracts/build/E.mligo",
        "child": { "file": "../../test/contracts/build/F.mligo" },
        "child": { "file": "../../test/contracts/build/G.mligo" }
      }
    } |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "B.mligo" ; "-e" ; "f" ] ;
  [%expect{|
    { parameter unit ;
      storage int ;
      code { PUSH int 1 ;
             PUSH int 42 ;
             SWAP ;
             DUP ;
             DUG 2 ;
             ADD ;
             PAIR ;
             LAMBDA
               (pair (pair int int) (pair unit int))
               (pair (list operation) int)
               { UNPAIR ;
                 UNPAIR ;
                 DIG 2 ;
                 CDR ;
                 SWAP ;
                 DUG 2 ;
                 ADD ;
                 ADD ;
                 NIL operation ;
                 PAIR } ;
             SWAP ;
             APPLY ;
             SWAP ;
             EXEC } } |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; contract "D.mligo" ] ;
  [%expect {|
    const toto = ADD(E.toto ,
    C.B.titi)
    const fb = record[tata -> 2 , tete -> 3 , titi -> 1 , toto -> toto]
    const main = lambda (#6) return let #8 = #6 in  match #8 with
                                                     | ( p , s ) ->
                                                     let s = ADD(ADD(p , s) ,
                                                     toto) in ( LIST_EMPTY() , s ) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "mini-c" ; contract "D.mligo" ] ;
  [%expect{|
let ../../test/contracts/build/A.mligo_toto = L(1) in
let ../../test/contracts/build/B.mligo_toto = L(32) in
let ../../test/contracts/build/B.mligo_titi =
  ADD(../../test/contracts/build/A.mligo_toto , L(42)) in
let ../../test/contracts/build/B.mligo_f =
  fun #1 ->
  (let #6 = #1 in
   let (#10, #11) = #6 in
   let #2 = #10 in
   let x = #11 in
   let x =
     ADD(ADD(x , ../../test/contracts/build/A.mligo_toto) ,
         ../../test/contracts/build/B.mligo_titi) in
   PAIR(LIST_EMPTY() , x)) in
let ../../test/contracts/build/F.mligo_toto = L(44) in
let ../../test/contracts/build/G.mligo_toto = L(43) in
let ../../test/contracts/build/C.mligo_tata =
  ADD(../../test/contracts/build/A.mligo_toto ,
      ../../test/contracts/build/B.mligo_titi) in
let ../../test/contracts/build/C.mligo_foo =
  (../../test/contracts/build/B.mligo_f)@(PAIR(L(unit) , L(3))) in
let ../../test/contracts/build/E.mligo_toto = L(10) in
let ../../test/contracts/build/E.mligo_foo = L("bar") in
let toto =
  ADD(../../test/contracts/build/E.mligo_toto ,
      ../../test/contracts/build/C.mligo_B_titi) in
let fb = (L(1), toto, L(2), L(3)) in
let main =
  fun #4 ->
  (let #8 = #4 in
   let (#12, #13) = #8 in
   let p = #12 in
   let s = #13 in let s = ADD(ADD(p , s) , toto) in PAIR(LIST_EMPTY() , s)) in
L(unit) |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "D.mligo" ] ;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Cli_expect_tests.Cli_expect.Should_exit_good)
  Raised at Cli_expect_tests__Cli_expect.run_ligo_good in file "src/bin/expect_tests/cli_expect.ml", line 28, characters 7-29
  Called from Cli_expect_tests__Build_module_test.(fun) in file "src/bin/expect_tests/build_module_test.ml", line 132, characters 2-63
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  An internal error ocurred. Please, contact the developers.
  Corner case: ../../test/contracts/build/C.mligo_B_titi not found in env. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; contract "cycle_A.mligo" ] ;
  [%expect {|
    Dependency cycle detected :
     `-- ../../test/contracts/build/cycle_A.mligo
        `-- ../../test/contracts/build/cycle_B.mligo
            `-- ../../test/contracts/build/cycle_C.mligo
                `-- ../../test/contracts/build/cycle_A.mligo |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "contract" ; contract "type_B.mligo" ] ;
  [%expect {|
    File "../../test/contracts/build/type_B.mligo", line 5, characters 5-6:
      4 | 	let s = s + 1 in
      5 | 	let p = p ^ "titi" in
      6 | 	([] : operation list), s
    :
    Warning: unused variable "p".
    Hint: replace it by "_p" to prevent this warning.

    { parameter string ;
      storage int ;
      code { CDR ; PUSH int 1 ; ADD ; NIL operation ; PAIR } } |}]

let%expect_test _ = 
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "tata" ; "--init-file" ; contract "C.mligo" ] ;
  [%expect {| 44 |}]

let%expect_test _ = 
  run_ligo_good [ "run" ; "test" ;  contract "C1.mligo" ] ;
  [%expect {|
  Everything at the top-level was executed.
  - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "run" ; "test" ; contract "C_test.mligo" ] ;
  [%expect {|
    Everything at the top-level was executed.
    - test exited with value (). |}]

let%expect_test _ =
  run_ligo_good [ "compile" ; "expression" ; "cameligo" ; "x" ; "--init-file" ; contract "Xmain.mligo" ] ;
  [%expect {|
    { 1 ; 2 ; 3 } |}]
