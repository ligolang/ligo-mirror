(* Driving the standalone preprocessor for ReasonLIGO *)

module type CONFIG = Preprocessor.Config.S

module Config : CONFIG = Preprocessing_reasonligo.Config
module PreprocMainGen  = Preprocessing_shared.PreprocMainGen
module PreprocMain     = PreprocMainGen.Make (Config)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preprocess ()
