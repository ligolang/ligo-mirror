(* Strings containing OCaml code need to be escaped before printed out
   to the terminal, but OCaml escaping function for strings escapes
   the double quotes, so we need to unescape those. *)

let escape s =
  let escaped = String.escaped s in
  let regexp = Str.regexp "\\\\\"" in
  Str.global_replace regexp "\"" escaped

(* Displaying code snippets in error messages *)

let fprintf = Format.fprintf

let print_code ppf (region : Region.t) (input_line : unit -> string) =
  let is_dumb    = match Sys.getenv_opt "TERM" with
                     Some value -> value = "dumb"
                   | None -> false
  and start      = region#start#line
  and start_offs = region#start#offset `Byte
  and stop       = region#stop#line
  and stop_offs  = region#stop#offset `Byte in
  let rec loop_over_lines current start stop =
    try
      let current = current + 1
      and line    = input_line () in
      let width   = String.length line in
      let () =
        if start - 1 <= current && current < stop + 2 then
         let () = fprintf ppf "%3i" current in
         if start <= current && current <= stop then
           if start < current && current < stop then
             fprintf
               ppf
               (if is_dumb then " | %s\n%!"
                else " | \027[1m\027[31m%s\027[0m\n%!")
               line
           else
             if current = start then
               let before = String.sub line 0 start_offs in
               fprintf ppf " | %s" before;
               if current = stop then
                 let between =
                   if start_offs = stop_offs then
                     String.sub line start_offs 1
                   else
                     String.sub line start_offs (stop_offs - start_offs) in
                 let between = escape between
                 and after =
                   if start_offs = stop_offs then
                     String.sub line (stop_offs + 1) (width - stop_offs -1)
                   else
                     String.sub line stop_offs (width - stop_offs) in
                 let after = escape after
                 in fprintf
                      ppf
                      (if is_dumb then "%s%!%s\n"
                       else "\027[1m\027[31m%s\027[0m%!%s\n")
                      between
                      after
               else
                 let after =
                   String.sub line start_offs (width - start_offs) in
                 let after = escape after in
                 if is_dumb then
                   fprintf ppf "%s%!\n" after
                 else
                   fprintf ppf "\027[1m\027[31m%s\027[0m%!\n" after
             else
               if current = stop then
                 let before = String.sub line 0 stop_offs |> escape in
                 let after  = String.sub line stop_offs (width - stop_offs) in
                 let after  = escape after in
                 fprintf ppf " | ";
                 if is_dumb then
                   fprintf ppf "%s%!%s\n" before after
                 else
                   fprintf ppf "\027[1m\027[31m%s\027[0m%!%s\n" before after
               else ()
           else fprintf ppf " | %s\n" line
      in if current < stop + 2 then
          loop_over_lines current start stop
    with Stdlib.Invalid_argument _msg -> () (* TODO: Report to maintainers? *)
       | Stdlib.End_of_file -> () (* Normal exit *)
    in loop_over_lines 0 start stop

let pp ppf : Location.t -> unit = function
  Virtual _ as loc ->
    Location.pp ppf loc
| File region ->
    if region#file <> "" then
      fprintf ppf "%s:\n" (region#to_string `Byte);
    try
      let in_chan = open_in region#file in
      let result = print_code ppf region (fun () -> input_line in_chan) in
      close_in in_chan;
      result
    with Sys_error _msg -> () (* TODO: Report to maintainers? *)

let pp_lift ppf r = pp ppf (File r)
