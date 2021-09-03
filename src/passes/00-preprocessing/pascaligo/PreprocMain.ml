(* Driving the standalone preprocessor for PascaLIGO *)

module type CONFIG = Preprocessor.Config.S

module Config : CONFIG = Preprocessing_pascaligo.Config
module PreprocMainGen  = Preprocessing_shared.PreprocMainGen
module PreprocMain     = PreprocMainGen.Make (Config)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preprocess ()
