(* This module is meant to be used by clients of the library to create
   standalone preprocessors tailored to their conventions. It is also
   internally used by PreprocMain.ml with default settings, for
   testing purposes. *)

(* The functor *)

module Make (Parameters : CLI.PARAMETERS) :
  sig
    type cli_status =
      Ok
    | Info  of string
    | Error of string

    val check_cli  : unit -> cli_status

    type output = {out : string; err : string}

    val preprocess : unit -> output * API.result
  end
