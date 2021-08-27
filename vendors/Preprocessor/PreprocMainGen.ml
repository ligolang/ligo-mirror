(* This module is meant to be used by clients of the library to create
   standalone preprocessors tailored to their conventions. It is also
   internally used by PreprocMain.ml with default settings, for
   testing purposes. *)

(* All exits *)

let red_exit msg =
  Printf.eprintf "\027[31m%s\027[0m\n%!" msg; exit 1

let cli_error msg =
  red_exit (Printf.sprintf "Command-line error: %s" msg)

let print_and_quit msg =
  print_string msg; flush stdout; exit 0

(* The functor *)

module Make (Parameters : CLI.PARAMETERS) =
  struct
    module Config  = Parameters.Config
    module Options = Parameters.Options
    module Scan    = API.Make (Config) (Options)

    (* Checking for errors and valid exits *)

    let check_cli () =
      match Parameters.Status.status with
        `SyntaxError  msg
      | `WrongFileExt msg
      | `FileNotFound msg -> cli_error msg
      | `Help         buf
      | `CLI          buf -> print_and_quit (Buffer.contents buf)
      | `Version      ver -> print_and_quit (ver ^ "\n")
      | `Done             -> ()

    (* Calling the preprocessor on the input file *)

    let preprocess () : API.result =
      let preprocessed =
        match Options.input with
               None -> Scan.from_channel stdin
        | Some path -> Scan.from_file path in
      let () =
        match preprocessed with
          Stdlib.Ok (buffer, _) ->
            if Options.show_pp then
              Printf.printf "%s\n%!" (Buffer.contents buffer)
        | Error (Some buffer, msg) ->
            if Options.show_pp then
              Printf.printf "%s\n%!" (Buffer.contents buffer);
            let out = Scan.format_error msg in
            Printf.eprintf "\027[31m%s\027[0m%!" out
        | Error (None, msg) ->
            let out = Scan.format_error msg in
            Printf.eprintf "\027[31m%s\027[0m%!" out
      in preprocessed
  end
