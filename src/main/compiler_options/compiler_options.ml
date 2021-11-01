open Environment

type module_resolutions = string list Simple_utils.Map.String.t option

type t = {
  init_env : Ast_typed.environment ;
  infer : bool ;
  libs : string list ;
  test : bool ;
  protocol_version : Protocols.t ;
  module_resolutions : module_resolutions;
}

let make : 
  ?infer : bool ->
  ?libs:string list ->
  ?protocol_version:Protocols.t ->
  ?test:bool -> 
  ?module_resolutions:module_resolutions -> unit -> t =
  fun 
    ?(infer = false)
    ?(libs = ([]:string list))
    ?(protocol_version=Protocols.current)
    ?(test = false) 
    ?(module_resolutions = None) () ->
      { init_env = if test then default_with_test protocol_version else default protocol_version;
        infer;
        libs ;
        protocol_version;
        test ;
        module_resolutions ;
      }
