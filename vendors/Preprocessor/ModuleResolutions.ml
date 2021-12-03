module SMap = Simple_utils.Map.String

type inclusion_list = string list

type dependency_list = string list

type lock_file = {
  root : string ;
  node : dependency_list SMap.t
}

type t = {
  root_path   : string ;
  resolutions : (string * inclusion_list) list ;
}

let traverse xs =
  List.fold_left
    (fun acc x -> 
      Option.bind acc 
        (fun acc -> Option.map (fun x -> x :: acc) x))
    (Some [])
    xs

module JsonHelpers = struct
  let string json = 
    match json with
      `String s -> Some s
    | _ -> None 
  let list json = 
    match json with
      `List l -> Some l
    | _ -> None
  let string_list json = 
    let l = list json in
    match l with
      Some l ->
        let strings = List.map string l in
        traverse strings
    | None -> None

  let from_file_opt file =
    try Some (Yojson.Basic.from_file file) with _ -> None
end

module Path = struct
  type t = Fpath.t option

  let v : string -> t = fun s -> try Some (Fpath.v s) with _ -> None

  let dir_sep = Fpath.dir_sep

  let segs : t -> string list option = fun t -> Option.map Fpath.segs t

  let is_prefix : string option -> string option -> bool = 
    fun prefix p ->
      let prefix = Option.bind prefix v in
      let p      = Option.bind p v in
      match prefix, p with
        Some prefix, Some p -> 
          Fpath.is_prefix prefix p
      | _ -> false 

  let is_abs : t -> bool = function Some p -> Fpath.is_abs p | None -> false

  let normalize : t -> t = Option.map Fpath.normalize

  let to_string_opt : t -> string option = Option.map Fpath.to_string

end

let clean_installation_json installation_json =
  match installation_json with
    None -> None
  | Some installation_json -> let open Yojson.Basic in
    let keys = Util.keys installation_json in
    let values = Util.values installation_json in
    List.fold_left2 
      (fun m key value -> 
        Option.bind m (fun m -> 
          let value = JsonHelpers.string value in
          Option.map (fun value -> SMap.add key value m) value
        )) 
      (Some SMap.empty) 
      keys values

let clean_lock_file_json lock_json =
  match lock_json with
    None -> None
  | Some lock_json -> let open Yojson.Basic in
    let root = Util.member "root" lock_json |> JsonHelpers.string in
    let node = Util.member "node" lock_json in
    let keys = Util.keys node in
    let values = Util.values node in
    let node = List.fold_left2
      (fun m key value ->
        let dependencies = Util.member "dependencies" value in  
        let dependencies = JsonHelpers.string_list dependencies in
        Option.bind m (fun m -> 
          Option.map (fun dependencies -> SMap.add key dependencies m) dependencies
        )
      )
      (Some SMap.empty)
      keys values in
    match (root,node) with
      Some root, Some node -> Some ({ root ; node })
    | _ -> None

let resolve_path installation path = SMap.find_opt path installation

let resolve_paths installation graph =
  SMap.fold (fun k v xs ->
    Option.bind xs (fun xs -> 
      let resolved = traverse (List.map (resolve_path installation) v) in
      let k = resolve_path installation k in
      (match k,resolved with
        Some k, Some resolved ->
          let paths = List.sort String.compare resolved in 
          Some ((k, paths) :: xs)
      | _ -> None) 
    )
  ) graph (Some [])

let find_dependencies (lock_file : lock_file) = 
  let root = lock_file.root in
  let node = lock_file.node in
  let rec dfs dep graph =
    let deps = SMap.find_opt dep node in
    match deps,graph with
      Some deps,Some graph -> 
        let graph = SMap.add dep deps graph in
        List.fold_left (fun graph dep -> dfs dep graph) (Some graph) deps
    | _ -> None
  in
  dfs root (Some SMap.empty)
  
let installation_json_path path = 
  path ^ Path.dir_sep ^ "_esy/default/installation.json"

let lock_file_path path =
  path ^ Path.dir_sep ^ "esy.lock/index.json"

let make project_path : t option =
  let installation_json = installation_json_path project_path 
    |> JsonHelpers.from_file_opt
    |> clean_installation_json
  in
  let lock_file_json = lock_file_path project_path 
    |> JsonHelpers.from_file_opt
    |> clean_lock_file_json
  in
  (match installation_json,lock_file_json with
    Some installation_json, Some lock_file_json ->
      let dependencies = find_dependencies lock_file_json in
      Option.bind dependencies (fun dependencies ->
        let resolutions = resolve_paths installation_json dependencies in
        let root_path = resolve_path installation_json lock_file_json.root in
        (match root_path, resolutions with
          Some root_path, Some resolutions -> Some { root_path ; resolutions = resolutions }
        | _ -> None)
      )
  | _ -> None)

let get_root_inclusion_list (module_resolutions : t option) = 
  match module_resolutions with
  | Some module_resolutions  ->
    let root_path = module_resolutions.root_path in
    let root_inclusion_list = List.find_opt 
      (fun (path,_) -> path = root_path) 
      module_resolutions.resolutions in
    (match root_inclusion_list with 
      Some (_,root_inclusion_list) -> root_inclusion_list
    | None -> [])
  | None -> []

let get_absolute_path path = 
  let path' = Path.v path in
  if Path.is_abs path' then Some path
  else 
    Path.v ((Sys.getcwd ()) ^ Path.dir_sep ^ path) 
      |> Path.normalize 
      |> Path.to_string_opt

let get_inclusion_list ~file (module_resolutions : t option) =
  match module_resolutions with
    Some module_resolutions ->
      let path = get_absolute_path file in
      (match List.find_opt (fun (mod_path, _) -> 
        Path.is_prefix (Some mod_path) path
        ) module_resolutions.resolutions 
      with
        Some (_,paths) -> paths
      | None -> []
      )
  | None -> []

(* the function [find_external_file] specifically resolves files
   for ligo packages downloaded via esy.

   the [dirs] contains a list of paths. 
   esy package path/dir is of the form {package-name}__{version}__{hash}
   e.g. /path/to/esy/cache/ligo_list_helpers__1.0.0__bf074147

   a ligo package will be used in #import or #include,
   e.g. #import "ligo-list-helpers/list.mligo" "ListHelpers"

   To correctly resolve the path for #import or #include we split the 
   path into 2 parts i.e. the package name & rest of the path
   e.g. "ligo-list-helpers/list.mligo" - 
    package name = ligo-list-helpers
    rest of path = list.mligo

   We find the path with the longest prefix, once the package path is
   identified, the file path is package path / rest of path
*)
let find_external_file ~file ~inclusion_list = 
  let starts_with ~prefix s =
    let s1 = String.length prefix in
    let s2 = String.length s in
    let rec aux i =
      if i >= s1 || i >= s2 then true
      else if prefix.[i] = s.[i] then aux (i + 1)
      else false
    in
    s2 >= s1 && aux 0
  in
  let segs = Path.segs (Path.v file) in
  Option.bind segs (fun segs -> 
    match segs with
      pkg_name::file_name -> 
        let file_name = String.concat Filename.dir_sep (List.tl segs) in
        let pkg_name = (List.hd segs) in
        let normalized_pkg_name = pkg_name
          |> String.split_on_char '_'
          |> String.concat "__"
          |> String.split_on_char '-' 
          |> String.concat "_" in
        let dir = List.find_opt (fun dir ->
          let basename = Filename.basename dir in
          let found = starts_with ~prefix:normalized_pkg_name basename  in
          if not found 
          then starts_with ~prefix:pkg_name basename 
          else found
        ) inclusion_list in
        Option.map (fun dir -> 
          let path = dir ^ Filename.dir_sep ^ file_name in
          path
        ) dir
    | _ -> None
  )
