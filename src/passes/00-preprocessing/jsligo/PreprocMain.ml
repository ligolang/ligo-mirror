(* Driving the standalone preprocessor for JsLIGO *)

module type CONFIG = Preprocessor.Config.S

module Config : CONFIG = Preprocessing_jsligo.Config
module Parameters      = Preprocessor.CLI.Make (Config)
module MainGen         = Preprocessor.PreprocMainGen.Make (Parameters)

let () =
  let open MainGen in
  match check_cli () with
    MainGen.Ok ->
      let {out; err}, _ = preprocess ()
      in Printf.printf  "%s%!" out;
         Printf.eprintf "%s%!" err
  | Info  msg -> Printf.printf "%s\n%!" msg
  | Error msg -> Printf.eprintf "%s\n%!" msg
