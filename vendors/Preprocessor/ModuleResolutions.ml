module List = Simple_utils.List
module Map = Simple_utils.Map

(* TODO: review one last time & try to get rid of failwith's *)
  
type t = (string * string list) list

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
      |> String.split_on_char '-' 
      |> String.concat "_" in
    let dir = List.find ~f:(fun dir ->
      let basename = (Fpath.basename @@ Fpath.v dir) in
      starts_with ~prefix:pkg_name basename 
    ) dirs in
    Option.map (fun dir -> 
      let path = dir ^ Filename.dir_sep ^ file_name in
      (path, open_in path)
    ) dir
  else None

let find_root_dependencies lock_file = 
  let open Yojson.Basic.Util in
  let root = member "root" lock_file |> JsonHelpers.string in
  let node = member "node" lock_file in
  JsonHelpers.string_list @@ member "dependencies" @@ member root node

let get_root_inclusion_list project_path = 
  match project_path with
  | Some project_path  ->
    let open Fpath in 
    let project_path = Fpath.v project_path in
    let installation_json = Yojson.Basic.from_file 
      (Fpath.to_string @@ (project_path // (Fpath.v "_esy/default/installation.json"))) in
    let lock_file_json = Yojson.Basic.from_file 
      (Fpath.to_string @@ (project_path // (Fpath.v "esy.lock/index.json"))) in
    let root_dependencies = find_root_dependencies lock_file_json in
    let resolve p =
      Yojson.Basic.Util.member p installation_json |> JsonHelpers.string
    in
    List.map ~f:resolve root_dependencies
  | None -> []
