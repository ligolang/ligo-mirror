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

let resolve_paths installation graph =
  let resolve p =
    Yojson.Basic.Util.member p installation |> JsonHelpers.string
  in
  Map.String.fold (fun k v xs ->
    let paths = List.sort ~compare:String.compare @@ (List.map ~f:resolve v) in 
    ((resolve k), paths) :: xs
  ) graph []


(* string -> string list *)
(* 

    ligo-main@link-dev:./esy.json ->    [ "temp-ligo-bin@0.28.1@d41d8cd9", "ligo-list-helpers@1.0.1@d41d8cd9", "ligo-foo@1.0.4@d41d8cd9" ]
    temp-ligo-bin@0.28.1@d41d8cd9 ->    [  ]
    ligo-list-helpers@1.0.1@d41d8cd9 -> [ temp-ligo-bin@0.25.0@d41d8cd9 ]
    ligo-foo@1.0.4@d41d8cd9 ->          [ "temp-ligo-bin@0.25.2@d41d8cd9", "ligo-set-helpers@1.0.2@d41d8cd9", "ligo-list-helpers@1.0.0@d41d8cd9" ]
    ligo-set-helpers@1.0.2@d41d8cd9 ->  [ "temp-ligo-bin@0.25.0@d41d8cd9" ]
    ligo-list-helpers@1.0.0@d41d8cd9 -> [ temp-ligo-bin@0.25.0@d41d8cd9 ]

    {
      /home/melwyn95/projects/ligo-pkg-mgmnt/ligo-main -> [
        /home/melwyn95/.esy/source/i/ligo_list_helpers__1.0.1__6233bebd,
        ...
      ]
      ...
    }

    main.mligo: #import "ligo-list-helpers/list.mligo" "ListExt"

    /home/melwyn95/.esy/source/i/ligo_foo__1.0.4__f8f13fa1/foo.mligo
*)


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
  dfs root dep_graph
  
let make project_path =
  match project_path with
  | Some project_path  ->
    let open Fpath in 
    let project_path = Fpath.v project_path in
    let installation_json = Yojson.Basic.from_file 
      (Fpath.to_string @@ (project_path // (Fpath.v "_esy/default/installation.json"))) in
    let lock_file_json = Yojson.Basic.from_file 
      (Fpath.to_string @@ (project_path // (Fpath.v "esy.lock/index.json"))) in
    let dependencies = find_dependencies lock_file_json in
    Some (resolve_paths installation_json dependencies)
  | None -> None

let get_absolute_path path = 
  let path' = Fpath.v path in
  if Fpath.is_abs path' then path
  else Fpath.v ((Sys.getcwd ()) ^ Fpath.dir_sep ^ path) |> Fpath.normalize |> Fpath.to_string

let get_includes path module_resolutions =
  match module_resolutions with
    Some module_resolutions ->
      let path = get_absolute_path path in
      (match List.find ~f:(fun (mod_path, _) -> 
          Fpath.is_prefix (Fpath.v mod_path) (Fpath.v path)
        ) module_resolutions 
      with
        Some (_,paths) -> paths
      | None -> []
      )
  | None -> []
