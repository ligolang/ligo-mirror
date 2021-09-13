(* This module is meant to be used by clients of the library to create
   standalone preprocessors tailored to their conventions. It is also
   internally used by PreprocMain.ml with default settings, for
   testing purposes. All functions are pure. *)

(* Vendor dependencies *)

module Std = Simple_utils.Std

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

    let preprocess () : Std.t * API.result =
      let preprocessed =
        match Options.input with
               None -> Scan.from_channel stdin
        | Some path -> Scan.from_file path in
      let std = Std.empty in
      let std =
        match preprocessed with
          Stdlib.Ok (text, _) ->
            if Options.show_pp then Std.add_out text std else std
        | Error (Some text, msg) ->
            let std =
              if Options.show_pp then Std.add_out text std else std
            in Std.add_err (Std.redden (Scan.format_error msg)) std
        | Error (None, msg) ->
            Std.add_err (Std.redden (Scan.format_error msg)) std in
      let std = Std.(add_out "" std |> add_err "")
      in std, preprocessed
  end
