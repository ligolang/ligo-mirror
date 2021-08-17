(* Driving the standalone preprocessor for CameLIGO *)

module Config         = Preprocessing_cameligo.Config
module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module PreprocMain    = PreprocMainGen.Make (Config)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preproc ()
