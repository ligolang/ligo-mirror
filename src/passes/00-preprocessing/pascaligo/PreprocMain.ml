(* Driving the standalone preprocessor for PascaLIGO *)

module Config         = Preprocessing_pascaligo.Config
module PreprocMainGen = Preprocessing_shared.PreprocMainGen
module PreprocMain    = PreprocMainGen.Make (Config)

let () = PreprocMain.check_cli ()
let () = PreprocMain.preproc ()
