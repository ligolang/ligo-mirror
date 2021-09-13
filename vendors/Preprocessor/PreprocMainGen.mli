(* This module is meant to be used by clients of the library to create
   standalone preprocessors tailored to their conventions. It is also
   internally used by PreprocMain.ml with default settings, for
   testing purposes. *)

module Std = Simple_utils.Std

module Make (Parameters : CLI.PARAMETERS) :
  sig
    (* Checking the CLI *)

    type cli_status =
      Ok
    | Info  of string
    | Error of string

    val check_cli  : unit -> cli_status

    (* Running the preprocessor *)

    val preprocess : unit -> Std.t * API.result
  end
