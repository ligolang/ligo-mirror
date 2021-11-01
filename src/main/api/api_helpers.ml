open Simple_utils.Display
open Simple_utils

module Trace = Simple_utils.Trace

let warn_str (display_format:ex_display_format) (a: 'a list) : string =
  let (Ex_display_format t) = display_format in
  match t with
  | Human_readable | Dev as t ->
     Format.asprintf "%a\n" (Simple_utils.PP_helpers.list_sep (Main_errors.Formatter.error_format.pp ~display_format:t) (Simple_utils.PP_helpers.tag "")) a
  | Json -> let json = List.map ~f:Main_errors.Formatter.error_format.to_json a in
            let s = Yojson.Safe.pretty_to_string @@ `List json in
            Format.asprintf "%s\n" s

let toplevel : ?werror:bool -> display_format:ex_display_format -> displayable -> (unit -> Main_warnings.all list) -> ('value, _) result -> _ =
  fun ?(werror=false) ~display_format disp warns value ->
    let (Ex_display_format t) = display_format in
    let as_str : string =
      match t with
      | Human_readable -> convert ~display_format:t disp ;
      | Dev -> convert ~display_format:t disp ;
      | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t disp
    in
    let warns = warns () in
    let warns = List.map warns ~f:(fun value ->
      match t with
        ( Human_readable | Dev) as s -> convert ~display_format:s (Displayable {value;format=Main_warnings.format})
        | Json -> Yojson.Safe.pretty_to_string @@ convert ~display_format:t (Displayable {value;format=Main_warnings.format})) in        
    let warns_str = String.concat "\n" warns in
    if not (List.is_empty warns) && werror then
        Error (warns_str,warns_str)
    else
    match value with
    | Ok _ -> Ok (as_str,warns_str)
    | Error _ -> Error (as_str,warns_str)

let format_result : ?werror:bool -> display_format:ex_display_format -> 'value format -> (unit -> Main_warnings.all list) -> (raise:Main_errors.all Trace.raise -> 'value) -> _ =
  fun ?(werror=false) ~display_format value_format warns value ->
    let format = bind_format value_format Main_errors.Formatter.error_format in
    let value = Trace.to_stdlib_result value in
    toplevel ~werror ~display_format (Displayable {value ; format}) warns value

module ModuleResolutions = struct
  type t = (string * string) list
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
    (resolve root, Map.String.fold (fun k v m ->
      Map.String.add (resolve k) (List.map ~f:resolve v) m
    ) graph Map.String.empty)

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
    
  let make ~installation ~lock =
    match installation, lock with
    | Some installation_json, Some lock_file  ->
      let installation_json = Yojson.Basic.from_file installation_json in
      let lock_file_json = Yojson.Basic.from_file lock_file in
      let root, dependencies = find_dependencies lock_file_json in
      Some (resolve_paths installation_json dependencies root)
    | Some _, None
    | None, Some _ -> 
      failwith "only one of installation.json or esy.lock/index.json is provided, please provide both"
    | None, None -> None

end