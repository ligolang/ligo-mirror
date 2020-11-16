(* Functor to build a standalone parser with printers *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* Generic signature of tokens *)

module type TOKEN =
  sig
    type token
    type t = token

    val to_lexeme : token -> string
    val to_string : offsets:bool -> [`Byte | `Point] -> token -> string
    val to_region : token -> Region.t
    val is_eof    : token -> bool

    val eof       : Region.t -> token
  end

(* Generic signature of input lexers *)

module type LEXER =
  sig
    module Token : TOKEN
    type token = Token.t

    type message = string Region.reg

    val scan : Lexing.lexbuf -> (token, message) Stdlib.result

    type window = <
      last_token    : token option;
      current_token : token           (* Including EOF *)
    >

    val get_window : (unit -> window option) ref
  end

(* The signature generated by Menhir with an additional type
   definition for [ast]. *)

module type PARSER =
  sig
    type token
    type tree

    (* The monolithic API. *)

    exception Error

    val main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> tree

    (* The incremental API. *)

    module MenhirInterpreter :
      sig
        include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
                with type token = token
      end

    module Incremental :
      sig
        val main :
          Lexing.position -> tree MenhirInterpreter.checkpoint
      end
  end

(* Parser errors for the Incremental API of Menhir *)

module type PAR_ERR =
  sig
    val message : int -> string
  end

(* The functor integrating the parser with its errors *)

module Make (Lexer: LEXER)
            (Parser: PARSER with type token = Lexer.Token.token) =
  struct
    module Token = Lexer.Token
    type token = Lexer.token

    type message = string Region.reg

    type 'src parser =
      'src -> (Parser.tree, message) Stdlib.result

    (* Errors and error messages *)

    let get_window () : Lexer.window =
      let invalid_eof =
        object
          method last_token    = None
          method current_token = Token.eof Region.ghost
        end in
      match !Lexer.get_window () with
        None -> invalid_eof
      | Some window -> window

    (* THE MONOLITHIC API *)

    exception LexingError of message

    let mk_menhir_lexer lexer source =
      match lexer source with
        Stdlib.Ok token -> token
        (* Encoding due to the API generated by Menhir: *)
      | Stdlib.Error msg -> raise (LexingError msg)

    let mono_menhir lexbuf_of source =
      let lexbuf = lexbuf_of source in
      let menhir_lexer = mk_menhir_lexer Lexer.scan in
      try Stdlib.Ok (Parser.main menhir_lexer lexbuf) with
        (* See [mk_menhir_lexer]: *)
        LexingError msg -> Stdlib.Error msg
      | Parser.Error -> (* Menhir exception *)
          let window = get_window () in
          let region = Token.to_region window#current_token
          in Stdlib.Error Region.{value=""; region}

    let mono_from_lexbuf  = mono_menhir (fun x -> x)
    let mono_from_channel = mono_menhir Lexing.from_channel
    let mono_from_string  = mono_menhir Lexing.from_string

    type file_path = string

    let lexbuf_from_file path =
      try
        let in_chan = open_in path in
        let lexbuf  = Lexing.from_channel in_chan in
        let () =
          let open Lexing in
          lexbuf.lex_curr_p <-
            {lexbuf.lex_curr_p with pos_fname=path}
        in Ok (lexbuf, fun () -> close_in in_chan)
      with
        Sys_error msg -> Stdlib.Error (Region.wrap_ghost msg)

    let mono_from_file path =
      match lexbuf_from_file path with
        Stdlib.Error _ as err -> err
      | Ok (lexbuf, close) ->
          let tree = mono_menhir (fun x -> x) lexbuf
          in close (); tree

    (* THE INCREMENTAL API *)

    module Inter = Parser.MenhirInterpreter

    (* The call [state checkpoint] extracts the number of the current
       state out of a parser checkpoint. The case [None] denotes the
       case of an error state with an empty LR stack: Menhir does not
       know how to determine that state. Until this is fixed, we
       return [None] and a generic error message (see function
       [message] below.) *)

    let state checkpoint : int option =
      let stack = function
        Inter.HandlingError env -> Some (Inter.stack env)
      |                       _ -> None in
      let module Stream = MenhirLib.General in
      match stack checkpoint with
        None -> None
      | Some state ->
          let open Stream in
          match Lazy.force state with
            Nil -> None
          | Cons (Inter.Element (s,_,_,_), _) -> Some (Inter.number s)

    (* The parser has suspended itself because of a syntax error. *)

    exception ParsingError of string

    let failure (module ParErr : PAR_ERR) checkpoint =
      let msg =
        match state checkpoint with
          (* A MenhirLib limitation (see [state]). Work around. *)
          None -> "Syntax error."
        | Some state ->
            match ParErr.message state with
              (* Default error message (unfinished mapping) *)
              "<YOUR SYNTAX ERROR MESSAGE HERE>\n" ->
                Printf.sprintf "Syntax error #%i." state
            | msg -> msg
              (* Likely a build error, but we work around it: *)
            | exception Not_found -> "Syntax error."
      in raise (ParsingError msg)

    (* The parser has successfully produced a semantic value. *)

    let success v = v

    (* Incremental parsing *)

    let incr_menhir lexbuf_of (module ParErr : PAR_ERR) source =
      let lexbuf       = lexbuf_of source
      and menhir_lexer = mk_menhir_lexer Lexer.scan in
      let supplier     = Inter.lexer_lexbuf_to_supplier menhir_lexer lexbuf
      and failure      = failure (module ParErr) in
      let interpreter  = Inter.loop_handle success failure supplier in
      let module Incr  = Parser.Incremental in
      let parser       = Incr.main lexbuf.Lexing.lex_curr_p in
      let tree =
        try Stdlib.Ok (interpreter parser) with
        (* See [mk_menhir_lexer]: *)
          LexingError msg -> Stdlib.Error msg
        | ParsingError msg ->
            let window = get_window () in
            let region = Token.to_region window#current_token in
            let value  = msg ^ "\n"
            in Stdlib.Error Region.{value; region}
      in flush_all (); tree

    let incr_from_lexbuf  = incr_menhir (fun x -> x)
    let incr_from_channel = incr_menhir Lexing.from_channel
    let incr_from_string  = incr_menhir Lexing.from_string

    let incr_from_file (module ParErr : PAR_ERR) path =
      match lexbuf_from_file path with
        Stdlib.Error _ as err -> err
      | Ok (lexbuf, close) ->
          let tree = incr_from_lexbuf (module ParErr) lexbuf
          in close (); tree
  end
