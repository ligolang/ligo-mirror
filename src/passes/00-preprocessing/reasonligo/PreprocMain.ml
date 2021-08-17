(* Driving the standalone preprocessor for ReasonLIGO *)

module Config         = Preprocessing_reasonligo.Config
module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module PreprocMain    = PreprocMainGen.Make (Config)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preproc ()
