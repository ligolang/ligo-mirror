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

let resolve_paths installation graph root =
  let resolve p =
    Yojson.Basic.Util.member p installation |> JsonHelpers.string
  in
  (resolve root, Map.String.fold (fun k v xs ->
    let paths = List.sort ~compare:String.compare @@ (List.map ~f:resolve v) in 
    ((resolve k), paths) :: xs
  ) graph [])

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

let get_includes path (module_resolutions : Compiler_options.module_resolutions) =
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
  match module_resolutions with
    Some (_, module_resolutions) ->
    (match List.find ~f:(fun (mod_path, _) -> starts_with ~prefix:mod_path path) module_resolutions with
      Some (_,paths) -> paths
    | None -> []
    )
  | None -> []
