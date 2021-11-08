module List = Simple_utils.List
module Map = Simple_utils.Map
  
module JsonHelpers = struct
  let string json = 
    match json with
      `String s -> s
    | _ -> failwith "invalid json: not a string" 
  let list json = 
    match json with
      `List l -> l
    | _ -> failwith "invalid json: not a list"
  let string_list json = List.map ~f:string (list json)
end

type t = (string * string list) list 

let resolve_paths installation graph root =
  let resolve p =
    Yojson.Basic.Util.member p installation |> JsonHelpers.string
  in
  Map.String.fold (fun k v xs ->
    let paths = List.sort ~compare:String.compare @@ (List.map ~f:resolve v) in 
    ((resolve k), paths) :: xs
  ) graph []

let find_dependencies lock_file = 
  let open Yojson.Basic.Util in
  let root = member "root" lock_file |> JsonHelpers.string in
  let node = member "node" lock_file in
  let dep_graph = Map.String.empty in
  let rec dfs dep graph =
    let deps = member dep node |> member "dependencies" |> JsonHelpers.string_list in
    let graph = Map.String.add dep deps graph in
    List.fold_left ~f:(fun graph dep -> dfs dep graph) ~init:graph deps
  in
  root, dfs root dep_graph
  
let make project_path =
  match project_path with
  | Some project_path  ->
    let open Fpath in 
    let project_path = Fpath.v project_path in
    let installation_json = Yojson.Basic.from_file 
      (Fpath.to_string @@ (project_path // (Fpath.v "_esy/default/installation.json"))) in
    let lock_file_json = Yojson.Basic.from_file 
      (Fpath.to_string @@ (project_path // (Fpath.v "esy.lock/index.json"))) in
    let root, dependencies = find_dependencies lock_file_json in
    Some (resolve_paths installation_json dependencies root)
  | None -> None

let get_absolute_path path = 
  let path' = Fpath.v path in
  if Fpath.is_abs path' then path
  else Fpath.v ((Sys.getcwd ()) ^ Fpath.dir_sep ^ path) |> Fpath.normalize |> Fpath.to_string

let get_includes path module_resolutions =
  let path = get_absolute_path path in
  match module_resolutions with
    Some module_resolutions ->
    (match List.find ~f:(fun (mod_path, _) -> 
        Fpath.is_prefix (Fpath.v mod_path) (Fpath.v path)
      ) module_resolutions 
    with
      Some (_,paths) -> paths
    | None -> []
    )
  | None -> []
