(* Driving the standalone preprocessor for JsLIGO *)

module Config         = Preprocessing_jsligo.Config
module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module PreprocMain    = PreprocMainGen.Make (Config)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preproc ()
