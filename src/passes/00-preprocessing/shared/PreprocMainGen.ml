(* Driving the preprocessor *)

(* Vendors dependencies *)

module Config = Preprocessor.Config

(* The functor *)

module Make (Config : Config.S) =
  struct
    module Parameters   = Preprocessor.CLI.Make (Config)
    module Main         = Preprocessor.PreprocMainGen
    module Preprocessor = Main.Make (Parameters)

    (* All exits *)

    let print_in_red msg = Printf.eprintf "\027[31m%s\027[0m%!" msg

    let red_exit msg = print_in_red msg; exit 1

    let cli_error msg =
      red_exit (Printf.sprintf "Command-line error: %s\n" msg)

    let check_cli = Preprocessor.check_cli

    let preproc () : unit =
      match Preprocessor.preprocess () with
        Stdlib.Ok (buffer, _) ->
          Printf.printf "%s%!" (Buffer.contents buffer)
      | _ -> ()
  end
