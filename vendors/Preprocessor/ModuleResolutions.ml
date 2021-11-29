module List = Simple_utils.List
module Map = Simple_utils.Map

(* TODO: review one last time *)
  
type t = (string * string list) list

let traverse xs =
  List.fold_left xs
    ~init:(Some [])
    ~f:(fun acc x -> 
      Option.bind acc 
        (fun acc -> Option.map (fun x -> x :: acc) x))

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
        let strings = List.map ~f:string l in
        traverse strings
    | None -> None

  let from_file_opt file =
    try Some (Yojson.Basic.from_file file) with _ -> None
end

module SMap = Map.String

type lock_file = {
  root : string ;
  node : (string list) SMap.t
}

let clean_installation_json installation_json =
  match installation_json with
    None -> None
  | Some installation_json -> let open Yojson.Basic in
    let keys = Util.keys installation_json in
    let values = Util.values installation_json in
    Stdlib.List.fold_left2 
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
    let node = Stdlib.List.fold_left2
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

let resolve_paths installation graph =
  let resolve p = SMap.find_opt p installation
  in
  SMap.fold (fun k v xs ->
    Option.bind xs (fun xs -> 
      let resolved = traverse (List.map ~f:resolve v) in
      let k = resolve k in
      (match k,resolved with
        Some k, Some resolved ->
          let paths = List.sort ~compare:String.compare resolved in 
          Some ((k, paths) :: xs)
      | _ -> None) 
    )
  ) graph (Some [])


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
  let root = lock_file.root in
  let node = lock_file.node in
  let rec dfs dep graph =
    let deps = SMap.find_opt dep node in
    match deps,graph with
      Some deps,Some graph -> 
        let graph = SMap.add dep deps graph in
        List.fold_left ~f:(fun graph dep -> dfs dep graph) ~init:(Some graph) deps
    | _ -> None
  in
  dfs root (Some SMap.empty)
  
let installation_json_path path = 
  path ^ Fpath.dir_sep ^ "_esy/default/installation.json"

let lock_file_path path =
  path ^ Fpath.dir_sep ^ "esy.lock/index.json"

let make project_path =
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
        resolve_paths installation_json dependencies
      )
  | _ -> None)

let get_absolute_path path = 
  let path' = Fpath.v path in
  if Fpath.is_abs path' then path
  else Fpath.v ((Sys.getcwd ()) ^ Fpath.dir_sep ^ path) |> Fpath.normalize |> Fpath.to_string

let get_inclusion_list path module_resolutions =
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
(* TODO: review carefully one last time *)
let find_external_file file dirs = 
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
  let segs = Fpath.segs @@ Fpath.v file in
  if List.length segs > 1 then
    let file_name = String.concat Filename.dir_sep (List.tl_exn segs) in
    let pkg_name = (List.hd_exn @@ segs) 
      |> String.split_on_char '_'
      |> String.concat "__"
      |> String.split_on_char '-' 
      |> String.concat "_" in
    let dir = List.find ~f:(fun dir ->
      let basename = (Fpath.basename @@ Fpath.v dir) in
      starts_with ~prefix:pkg_name basename 
    ) dirs in
    Option.map (fun dir -> 
      let path = dir ^ Filename.dir_sep ^ file_name in
      path
    ) dir
  else None

let find_root_dependencies lock_file = 
  let root = lock_file.root in
  let node = lock_file.node in
  let dependencies = SMap.find_opt root node in
  match dependencies with
    Some dependencies -> dependencies
  | None -> []

let get_root_inclusion_list project_path = 
  match project_path with
  | Some project_path  ->
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
        let root_dependencies = find_root_dependencies lock_file_json in
        let resolve p =
          SMap.find_opt p installation_json
        in
        let dependencies_paths = List.map ~f:resolve root_dependencies in
        let dependencies_paths = List.fold_left 
          ~f:(fun acc path -> Option.bind acc (fun paths -> Option.map (fun path -> path :: paths) path)) 
          ~init:(Some []) dependencies_paths in
        (match dependencies_paths with
          Some dependencies_paths -> dependencies_paths
        | None -> [])
    | _ -> [])
  | None -> []
