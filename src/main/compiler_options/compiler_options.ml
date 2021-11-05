open Environment

type t = {
  init_env : Ast_typed.environment ;
  infer : bool ;
  libs : string list ;
  test : bool ;
  protocol_version : Protocols.t ;
  esy_project_path : string option ;
}

let make : 
  ?infer : bool ->
  ?libs:string list ->
  ?protocol_version:Protocols.t ->
  ?test:bool -> 
  ?esy_project_path:string -> unit -> t =
  fun 
    ?(infer = false)
    ?(libs = ([]:string list))
    ?(protocol_version=Protocols.current)
    ?(test = false) 
    ?(esy_project_path) () ->
      { init_env = if test then default_with_test protocol_version else default protocol_version;
        infer;
        libs ;
        protocol_version;
        test ;
        esy_project_path ;
      }
