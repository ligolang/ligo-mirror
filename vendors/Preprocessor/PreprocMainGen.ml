(* This module is meant to be used by clients of the library to create
   standalone preprocessors tailored to their conventions. It is also
   internally used by PreprocMain.ml with default settings, for
   testing purposes. *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

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
    module API = API.Make (Config) (Options)

    (* Checking for errors and valid exits *)

    let check_cli () =
      let open Parameters in
      match status with
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
               None -> API.from_channel stdin
        | Some path -> API.from_file path in
      let () =
        match preprocessed with
          Stdlib.Ok (buffer, _) ->
            if Options.show_pp then
              Printf.printf "%s\n%!" (Buffer.contents buffer)
        | Error (Some buffer, Region.{value; _}) ->
            if Options.show_pp then
              Printf.printf "%s\n%!" (Buffer.contents buffer);
            Printf.eprintf "\027[31m%s\027[0m%!" value
        | Error (None, Region.{value; _}) ->
            Printf.eprintf "\027[31m%s\027[0m%!" value
      in preprocessed
  end
