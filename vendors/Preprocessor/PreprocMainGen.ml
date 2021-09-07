(* This module is meant to be used by clients of the library to create
   standalone preprocessors tailored to their conventions. It is also
   internally used by PreprocMain.ml with default settings, for
   testing purposes. All functions are pure. *)

(* CLI errors *)

let cli_error msg = Printf.sprintf "\027[31m%s\027[0m" msg

(* The functor *)

module Make (Parameters : CLI.PARAMETERS) =
  struct
    module Config  = Parameters.Config
    module Options = Parameters.Options
    module Scan    = API.Make (Config) (Options)

    (* Checking for errors and valid exits *)

    type cli_status =
      Ok
    | Info  of string
    | Error of string

    let check_cli () : cli_status =
      match Parameters.Status.status with
        `SyntaxError  msg
      | `WrongFileExt msg
      | `FileNotFound msg -> Error (cli_error msg)
      | `Help         buf
      | `CLI          buf -> Info (Buffer.contents buf)
      | `Version      ver -> Info (ver ^ "\n")
      | `Done             -> Ok

    (* Calling the preprocessor on the input file *)

    type output = {out : string; err : string}

    let add_out out output =
      if output.out = "" then {output with out}
      else {output with out = output.out ^ "\n" ^ out}

    let add_err err output =
      if output.err = "" then {output with err}
      else {output with err = output.err ^ "\n" ^ err}

    let red string = Printf.sprintf "\027[31m%s\027[0m" string

    let preprocess () : output * API.result =
      let preprocessed =
        match Options.input with
               None -> Scan.from_channel stdin
        | Some path -> Scan.from_file path in
      let output = {out=""; err=""} in
      let output =
        match preprocessed with
          Stdlib.Ok (buffer, _) ->
            if Options.show_pp then
              add_out (Buffer.contents buffer) output
            else output
        | Error (Some buffer, msg) ->
            let output =
              if Options.show_pp then
                add_out (Buffer.contents buffer) output
              else output in
            add_err (red (Scan.format_error msg)) output
        | Error (None, msg) ->
            add_err (red (Scan.format_error msg)) output in
      let output = add_out "" output |> add_err ""
      in output, preprocessed
  end
