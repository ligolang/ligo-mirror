
(* Extracting information *)

let current_linenum lexbuf = Lexing.(lexbuf.lex_curr_p.pos_lnum)

let current_filename lexbuf = Lexing.(lexbuf.lex_curr_p.pos_fname)

(* Rolling back one lexeme _within the current semantic action_ *)

let rollback lexbuf =
  let open Lexing in
  let len = String.length (lexeme lexbuf) in
  let pos_cnum = lexbuf.lex_curr_p.pos_cnum - len in
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - len;
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_cnum}

(* Resetting file name and line number in the lexing buffer

   The call [reset ~file ~line lexbuf] modifies in-place the lexing
   buffer [lexbuf] so the lexing engine records that the file
   associated with [lexbuf] is named [file], and the current line is
   [line]. *)

let reset_file file lexbuf =
  let open Lexing in
  lexbuf.lex_curr_p <- {lexbuf.lex_start_p with pos_fname = file};
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_fname = file}

  let reset_line line lexbuf =
  assert (line >= 0);
  let open Lexing in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_lnum = line}

let reset_offset offset lexbuf =
  assert (offset >= 0);
  let open Lexing in
  let bol = lexbuf.lex_curr_p.pos_bol in
  lexbuf.lex_curr_p <- {lexbuf.lex_curr_p with pos_cnum = bol + offset}

type file_path = string

let reset ?file ?(line=1) ?offset lexbuf =
  let () =
    match file with
      Some file -> reset_file file lexbuf
    |      None -> () in
  let () = reset_line line lexbuf in
  match offset with
    Some offset -> reset_offset offset lexbuf
  |        None -> ()
