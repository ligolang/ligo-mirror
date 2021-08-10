(* Simple preprocessor based on cpp, to be processed by [ocamllex]. *)

{
(* START OF HEADER *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

(* General configuration *)

module type CONFIG = module type of Config

(* CLI options *)

module type OPTIONS = module type of Options

(* Functor *)

module type S =
  sig
    type file_path   = string
    type module_name = string
    type module_deps = (file_path * module_name) list
    type success     = Buffer.t * module_deps

    type message     = string Region.reg
    type error       = Buffer.t option * message

    type result      = (success, error) Stdlib.result
    type 'src preprocessor = 'src -> result

    (* Preprocessing from various sources *)

    val from_lexbuf  : Lexing.lexbuf preprocessor
    val from_channel : in_channel    preprocessor
    val from_string  : string        preprocessor
    val from_file    : file_path     preprocessor
  end

module Make (Config : CONFIG) (Options : OPTIONS) =
  struct
    (* Local dependencies *)

    open State

    (* FINDING FILES *)

    let open_file path state =
      let in_chan = open_in path in
      let state   = State.push_chan in_chan state
      in path, in_chan, state

    let find file_path state =
      let rec aux = function
          [] -> Stdlib.Error (Error.File_not_found file_path)
      | dir::dirs -> let path =
                      if dir = "." || dir = "" then file_path
                      else dir ^ Filename.dir_sep ^ file_path in
                    try Stdlib.Ok (open_file path state) with
                      Sys_error _ -> aux dirs
      in aux Options.dirs

    let find dir file state =
      let path =
        if dir = "." || dir = "" then file
        else dir ^ Filename.dir_sep ^ file in
      try Stdlib.Ok (open_file path state) with
        Sys_error _ ->
          let base = Filename.basename file in
          if base = file then find file state
          else Stdlib.Error (Error.File_not_found file)

    (* Extracting the region matched in a lexing buffer *)

    let mk_reg buffer =
      let start = Lexing.lexeme_start_p buffer |> Pos.from_byte
      and stop  = Lexing.lexeme_end_p buffer |> Pos.from_byte
      in Region.make ~start ~stop

    (* Rolling back one lexeme _within the current semantic action_ *)

    let rollback buffer =
      let open Lexing in
      let len = String.length (lexeme buffer) in
      let pos_cnum = buffer.lex_curr_p.pos_cnum - len in
      buffer.lex_curr_pos <- buffer.lex_curr_pos - len;
      buffer.lex_curr_p <- {buffer.lex_curr_p with pos_cnum}

    (* Utility functions *)

    let sprintf = Printf.sprintf

    (* STRING PROCESSING *)

    (* The value of [mk_str len p] ("make string") is a string of length
       [len] containing the [len] characters in the list [p], in
       reverse order. For instance, [mk_str 3 ['c';'b';'a'] = "abc"]. *)

    let mk_str (len: int) (p: char list) : string =
      let () = assert (len = List.length p) in
      let bytes = Bytes.make len ' ' in
      let rec fill i = function
        [] -> bytes
      | char::l -> Bytes.set bytes i char; fill (i-1) l
      in fill (len-1) p |> Bytes.to_string

    (* ERRORS *)

    (* IMPORTANT : Make sure the functions [fail] and [expr] remain the only
       ones raising [Error]. *)

    exception Error of (Buffer.t * string Region.reg)

    let format_error  ~msg (region: Region.t) =
      let file  = Options.input <> None in
      let reg   = region#to_string
                    ~file
                    ~offsets:Options.offsets
                    `Byte in
      let value = sprintf "%s:\n%s\n" reg msg
      in Region.{value; region}

    let fail state region error =
      let msg = Error.to_string error in
      let msg = format_error ~msg region
      in List.iter close_in state.chans;
         raise (Error (state.out, msg))

    let stop state buffer = fail state (mk_reg buffer)

    let find dir file region state =
      match find dir file state with
        Stdlib.Ok result -> result
      | Stdlib.Error err -> fail state region err

    let apply transform region state =
      match transform state with
        Stdlib.Ok state  -> state
      | Stdlib.Error err -> fail state region err

    let reduce_cond      = apply State.reduce_cond
    let extend cond mode = apply (State.extend cond mode)

    (* DIRECTIVES *)

    let directives = [
      "define";
      "elif";
      "else";
      "endif";
      "error";
      "if";
      "import";
      "include";
      "undef"
    ]

(* END OF HEADER *)
}

(* REGULAR EXPRESSIONS *)

let nl        = '\n' | '\r' | "\r\n"
let blank     = ' ' | '\t'
let digit     = ['0'-'9']
let natural   = digit | digit (digit | '_')* digit
let small     = ['a'-'z']
let capital   = ['A'-'Z']
let letter    = small | capital
let ident     = letter (letter | '_' | digit)*
let directive = '#' blank* (small+ as id)

(* Comment delimiters *)

let pascaligo_block_comment_opening = "(*"
let pascaligo_block_comment_closing = "*)"
let pascaligo_line_comment          = "//"

let cameligo_block_comment_opening = "(*"
let cameligo_block_comment_closing = "*)"
let cameligo_line_comment          = "//"

let reasonligo_block_comment_opening = "/*"
let reasonligo_block_comment_closing = "*/"
let reasonligo_line_comment          = "//"

let michelson_block_comment_opening = "/*"
let michelson_block_comment_closing = "*/"
let michelson_line_comment          = "#"

let block_comment_openings =
  pascaligo_block_comment_opening
| cameligo_block_comment_opening
| reasonligo_block_comment_opening
| michelson_block_comment_opening

let block_comment_closings =
  pascaligo_block_comment_closing
| cameligo_block_comment_closing
| reasonligo_block_comment_closing
| michelson_block_comment_closing

let line_comments =
  pascaligo_line_comment
| cameligo_line_comment
| reasonligo_line_comment
| michelson_line_comment

(* String delimiters *)

let pascaligo_string_delimiter  = "\""
let cameligo_string_delimiter   = "\""
let reasonligo_string_delimiter = "\""
let michelson_string_delimiter  = "\""
let jsligo_string_delimiter     = "\""

let string_delimiters =
  pascaligo_string_delimiter
| cameligo_string_delimiter
| reasonligo_string_delimiter
| michelson_string_delimiter
| jsligo_string_delimiter

(* RULES *)

(* The rule [scan] scans the input buffer for directives, strings,
   comments, blanks, new lines and end of file characters. As a
   result, either the matched input is copied to the buffer or not,
   depending on the compilation directives. Even if not in copy mode,
   new line characters are output. See README.md for the
   documentation.

   Important note: Comments and strings are recognised both in
   copy and skip mode, as GNU GCC does. *)

rule scan state = parse
  nl? eof { if state.trace = [] then state
            else stop state lexbuf Error.Missing_endif }
| nl      { proc_nl state lexbuf; scan state lexbuf }
| blank   { if state.mode = Copy then copy state lexbuf;
            scan state lexbuf }

  (* Strings *)

| string_delimiters {
    if state.mode = Copy then copy state lexbuf;
    let lexeme = Lexing.lexeme lexbuf in
    match Config.string with
      Some delimiter when delimiter = lexeme ->
        let state = in_string delimiter (mk_reg lexbuf) state lexbuf
        in scan state lexbuf
    | Some _ | None -> scan state lexbuf }

  (* Comments *)

| block_comment_openings {
    if state.mode = Copy then copy state lexbuf;
    let lexeme = Lexing.lexeme lexbuf in
    match Config.block with
      Some block when block#opening = lexeme ->
        let state = in_block block (mk_reg lexbuf) state lexbuf
        in scan state lexbuf
    | Some _ | None -> scan state lexbuf }

| line_comments {
    if state.mode = Copy then copy state lexbuf;
    let lexeme = Lexing.lexeme lexbuf in
    match Config.line with
      Some line when line = lexeme ->
        scan (in_line state lexbuf) lexbuf
    | Some _ | None -> scan state lexbuf }

(* Directive *)

| directive {
    let  region = mk_reg lexbuf in
    if   not (List.mem id directives)
    then begin
           if state.mode = Copy then copy state lexbuf;
           scan state lexbuf
         end
    else
    if   region#start#offset `Byte > 0
    then stop state lexbuf Error.Directive_inside_line
    else
    match id with
      "include" ->
        (* We first extract info about the current file so we can
           restore it after the #include is complete. *)

        let line = Lexing.(lexbuf.lex_curr_p.pos_lnum)
        and base = Filename.basename Lexing.(lexbuf.lex_curr_p.pos_fname)

        (* We read the string containing the name of the file to
           include. Note the first component [reg], which is the
           region in the file corresponding to the string, in case we
           will need to format an error message when the file is not
           found on the filesystem. *)

        and reg, incl_file = scan_include state lexbuf in

        if state.mode = Copy then
          (* If in copy mode, we establish the directory where the
             file to include is expected to reside. This directory may
             be relative to the current directory or not. See
             [incl_path] below. *)

          let incl_dir = Filename.dirname incl_file in

          (* We form the filesystem path to the current file. *)

          let path = mk_path state in

          (* We try to find the file to include. If missing, the
             exception [Error] is raised, with the value
             [Error.File_not_found]. Otherwise, we obtain a
             triplet. The first component [incl_path] may be different
             from [incl_dir] if the preprocessor is standalone, was
             given a list of directories with the command-line option
             [-I], and the file to include was not found relatively to
             the current directy, but from one of those given with
             [-I]. This is consistent with the behaviour of [cpp],
             insofar as we proofed it. The second component
             [incl_chan] is an input channel of type [in_channel],
             which has been registered with the [state], so we can
             close it when done. Finally, the last component is the
             new state to thread along. *)

          let incl_path, incl_chan, state = find path incl_file reg state in

          (* We are ready now to output the linemarker before
             including the file (as the rightmost flag [1] states). Of
             course we start at line 1 in the included file (as the
             leftmost flag [1] states). *)

          let () = print state (sprintf "\n# 1 %S 1\n" incl_path) in

          (* We prepare a lexing buffer from the input channel bound
             to the file to include. *)

          let incl_buf = Lexing.from_channel incl_chan in

          (* We instruct the lexing buffer just created that the
             corresponding file name is [incl_file] (recall that this
             may not be the fully qualified name, but the one given
             after #include. *)

          let () =
            let open Lexing in
            incl_buf.lex_curr_p <-
              {incl_buf.lex_curr_p with pos_fname = incl_file} in

          (* We make a variant copy of the current state meant to scan
             the file to include: we force the copy mode from the
             start, and we set an empty trace for conditional
             directives, so a conditional directive opened in the
             current file cannot be closed in the included file. *)

          let state' = {state with mode=Copy; trace=[]} in

          (* We perform a recursive call which will preprocess the
             file to include, because we thread the new state [state']
             we just created, after saving the include directory in
             it with a call to [push_dir]. *)

          let state' = scan (push_dir incl_dir state') incl_buf in

          (* After returning from the recursive call, we restore the
             state before the call, but we retain and commit some
             information from the state returned by the call: the
             symbol environment, the opened channels and the imports
             (#import of modules). The first because we want to enable
             an included file to contain #define and #undef
             directives, typically following the traditional design
             pattern to avoid double inclusions. The second because
             the included file may contain its own #include directives
             and therefore open new input channels, which will need
             closing when we are done. The third because the included
             file may contain dependencies from #import directives. *)

          let state = {state with env    = state'.env;
                                  chans  = state'.chans;
                                  import = state'.import} in

          (* We now have to prepare the linemarker that indicates that
             we returned from a file inclusion. First, we need the
             filesystem path to the current file [path], which we used
             earlier to locate the file to include. We format it to
             conform to the convention of [cpp]. *)

          let path = if path = "" || path = "." then base
                     else path ^ Filename.dir_sep ^ base in

          (* Finally we can output the linemarker. The rightmost flag
             is 2, to specify a return from an #include. The leftmost
             flag is the line number [line+1], which we extracted and
             kept safe earlier, before doing anything. *)

          let () = print state (sprintf "\n# %i %S 2\n" (line+1) path)

          (* We can now resume preprocessing the current file. *)

          in scan state lexbuf

        (* If in skip mode, we resume scanning. The #include and its
           argument will be missing in the output. *)

        else scan state lexbuf

    | "import" ->
        let reg, import_file, imported_module = scan_import state lexbuf in
        let state =
          if state.mode = Copy then
            let path = mk_path state in
            let import_path, _, state = find path import_file reg state
            in State.push_import import_path imported_module state
          else state
        in (proc_nl state lexbuf; scan state lexbuf)

    | "if" ->
        let bool =
          try Boolean.expr (if_expr state) lexbuf state.env with
            Boolean.Error -> stop state lexbuf Error.Parse_error in
        let mode  = if bool then Copy else Skip in
        let mode  = if state.mode = Copy then mode else Skip in
        let state = extend (If state.mode) mode region state
        in scan state lexbuf

    | "else" ->
        let ()    = skip_line state lexbuf in
        let mode  = match state.mode with
                      Copy -> Skip
                    | Skip -> last_mode state.trace in
        let state = extend Else mode region state
        in scan state lexbuf

    | "elif" ->
        let bool =
          try Boolean.expr (if_expr state) lexbuf state.env with
            Boolean.Error -> stop state lexbuf Parse_error in
        let mode  = if bool then Copy else Skip in
        let state =
          match state.mode with
            Copy -> extend (Elif Skip) Skip region state
          | Skip -> let old_mode = last_mode state.trace in
                    let new_mode = if old_mode = Copy then mode else Skip
                    in extend (Elif old_mode) new_mode region state
        in scan state lexbuf

    | "endif" ->
        skip_line state lexbuf;
        scan (reduce_cond region state) lexbuf

    | "define" ->
        let id, _ = variable state lexbuf in
        if state.mode = Copy
        then scan (env_add id state) lexbuf
        else scan state lexbuf

    | "undef" ->
        let id, _ = variable state lexbuf in
        if   state.mode = Copy
        then scan (env_rem id state) lexbuf
        else scan state lexbuf

    | "error" ->
        fail state region (Error.Error_directive (message [] lexbuf))

    | _ -> assert false
  }

| _ { if state.mode = Copy then copy state lexbuf;
      scan state lexbuf }

(* Scanning boolean expressions after #if and #elif *)

and if_expr state = parse
  blank+      { if_expr state lexbuf     }
| nl          { proc_nl state lexbuf;
                Boolean.EOL              }
| eof         { Boolean.EOL              }
| "true"      { Boolean.True             }
| "false"     { Boolean.False            }
| ident as id { Boolean.Ident id         }
| '('         { Boolean.LPAR             }
| ')'         { Boolean.RPAR             }
| "||"        { Boolean.OR               }
| "&&"        { Boolean.AND              }
| "=="        { Boolean.EQ               }
| "!="        { Boolean.NEQ              }
| "!"         { Boolean.NOT              }
| "//"        { if_expr_com state lexbuf }
| _ as c      { stop state lexbuf (Error.Invalid_character c) }

and if_expr_com state = parse
  nl  { proc_nl state lexbuf; Boolean.EOL }
| eof { Boolean.EOL                       }
| _   { if_expr_com state lexbuf          }

(* Support for #define and #undef *)

and variable state = parse
  blank+ { let id = symbol state lexbuf
           in skip_line state lexbuf; id }

and symbol state = parse
  ident as id { id, mk_reg lexbuf                      }
| _           { stop state lexbuf Error.Invalid_symbol }

(* Skipping all characters until the end of line or end of file. *)

and skip_line state = parse
  nl  { proc_nl state lexbuf   }
| eof { rollback lexbuf        }
| _   { skip_line state lexbuf }

(* For #error *)

and message acc = parse
  nl     { Lexing.new_line lexbuf;
           mk_str (List.length acc) acc }
| eof    { rollback lexbuf;
           mk_str (List.length acc) acc }
| blank* { message acc lexbuf           }
| _ as c { message (c::acc) lexbuf      }

(* Comments *)

and in_block block opening state = parse
  string_delimiters {
    if state.mode = Copy then copy state lexbuf;
    let lexeme = Lexing.lexeme lexbuf in
    match Config.string with
      Some delimiter when delimiter = lexeme ->
        let state = in_string delimiter (mk_reg lexbuf) state lexbuf
        in in_block block opening state lexbuf
    | Some _ | None -> in_block block opening state lexbuf }

| block_comment_openings {
    if state.mode = Copy then copy state lexbuf;
    let lexeme = Lexing.lexeme lexbuf in
    if block#opening = lexeme then
      let state = in_block block (mk_reg lexbuf) state lexbuf
      in in_block block opening state lexbuf
    else in_block block opening state lexbuf }

| block_comment_closings {
    if state.mode = Copy then copy state lexbuf;
    let lexeme = Lexing.lexeme lexbuf in
    if block#closing = lexeme then state
    else in_block block opening state lexbuf }

| nl  { proc_nl state lexbuf; in_block block opening state lexbuf }
| eof { let err = Error.Unterminated_comment block#closing
        in fail state opening err                                 }
| _   { if state.mode = Copy then copy state lexbuf;
        in_block block opening state lexbuf                       }

and in_line state = parse
  nl  { proc_nl state lexbuf; state                  }
| eof { rollback lexbuf; state                       }
| _   { if state.mode = Copy then copy state lexbuf;
        in_line state lexbuf                         }

(* #include *)

and scan_include state = parse
  blank+ { scan_include state lexbuf                    }
| '"'    { in_include (mk_reg lexbuf) [] 0 state lexbuf }
| _      { stop state lexbuf Error.Missing_filename     }

and in_include opening acc len state = parse
  '"'    { let region = Region.cover opening (mk_reg lexbuf)
           in region, end_include acc len state lexbuf       }
| nl     { stop state lexbuf Error.Newline_in_string         }
| eof    { fail state opening Error.Unterminated_string      }
| _ as c { in_include opening (c::acc) (len+1) state lexbuf  }

and end_include acc len state = parse
  nl     { Lexing.new_line lexbuf; mk_str len acc      }
| eof    { mk_str len acc                              }
| blank+ { end_include acc len state lexbuf            }
| _      { stop state lexbuf Error.Unexpected_argument }

(* #import *)

and scan_import state = parse
  blank+ { scan_import state lexbuf                    }
| '"'    { in_import (mk_reg lexbuf) [] 0 state lexbuf }
| _      { stop state lexbuf Error.Missing_filename    }

and in_import opening acc len state = parse
  '"'    { let imp_path = mk_str len acc
           in scan_module opening imp_path state lexbuf    }
| nl     { stop state lexbuf Error.Newline_in_string       }
| eof    { fail state opening Error.Unterminated_string    }
| _ as c { in_import opening (c::acc) (len+1) state lexbuf }

and scan_module opening imp_path state = parse
  blank+ { scan_module opening imp_path state lexbuf    }
| '"'    { in_module opening imp_path [] 0 state lexbuf }
| _      { stop state lexbuf Error.Missing_filename     }

and in_module opening imp_path acc len state = parse
  '"'    { end_module opening (mk_reg lexbuf) imp_path acc len state lexbuf }
| nl     { stop state lexbuf Error.Newline_in_string                        }
| eof    { fail state opening Error.Unterminated_string                     }
| _ as c { in_module opening imp_path (c::acc) (len+1) state lexbuf         }

and end_module opening closing imp_path acc len state = parse
  nl     { proc_nl state lexbuf;
           Region.cover opening closing, imp_path, mk_str len acc   }
| eof    { Region.cover opening closing, imp_path, mk_str len acc   }
| blank+ { end_module opening closing imp_path acc len state lexbuf }
| _      { stop state lexbuf Error.Unexpected_argument              }

and in_string delimiter opening state = parse
  string_delimiters {
           if state.mode = Copy then copy state lexbuf;
           let lexeme = Lexing.lexeme lexbuf in
           if lexeme = delimiter then state
           else in_string delimiter opening state lexbuf }
| nl     { fail state opening Error.Newline_in_string   }
| eof    { fail state opening Error.Unterminated_string }
| ['\000' - '\031'] as c
         { stop state lexbuf (Error.Invalid_character_in_string c) }
| _      { if state.mode = Copy then copy state lexbuf;
           in_string delimiter opening state lexbuf }

(* Entry point *)

and preproc state = parse
  eof { state }
| _   { let open Lexing in
        let ()   = rollback lexbuf in
        let name = lexbuf.lex_start_p.pos_fname in
        let ()   = if name <> "" then
                     print state (sprintf "# 1 %S\n" name)
        in scan state lexbuf }

{
(* START OF TRAILER *)

  (* The function [preproc] is a wrapper of [scan], which also checks that
     the trace is empty at the end.  Note that we discard the
     state at the end. *)

  type file_path   = string
  type module_name = string
  type module_deps = (file_path * module_name) list
  type success     = Buffer.t * module_deps

  type message     = string Region.reg
  type error       = Buffer.t option * message

  type result      = (success, error) Stdlib.result
  type 'src preprocessor = 'src -> result

  (* Preprocessing from various sources *)

  let from_lexbuf buffer =
    let path = Lexing.(buffer.lex_curr_p.pos_fname) in
    let state = {
      env    = Env.empty;
      mode   = Copy;
      trace  = [];
      out    = Buffer.create 80;
      chans  = [];
      incl   = [Filename.dirname path];
      import = []
      } in
    match preproc state buffer with
      state ->
        List.iter close_in state.chans;
        Stdlib.Ok (state.out, state.import)
    | exception Error (buffer, msg) ->
        Stdlib.Error (Some buffer, msg)

  let from_channel channel =
    Lexing.from_channel channel |> from_lexbuf

  let from_string string =
    Lexing.from_string string |> from_lexbuf

  let from_file name =
    try
      let lexbuf = open_in name |> Lexing.from_channel in
      let open Lexing in
        begin
          lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname=name};
          from_lexbuf lexbuf
        end
    with Sys_error msg ->
      Stdlib.Error (None, Region.wrap_ghost msg)

  end (* of functor [Make] *)
(* END OF TRAILER *)
}
