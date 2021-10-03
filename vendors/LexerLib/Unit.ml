(* Lexical units *)

module type S =
  sig
    module Options : Options.S

    type 'token lex_unit = [
      `Token     of 'token
    | `Markup    of Markup.t
    | `Directive of Directive.t
    ]

    type 'token t = 'token lex_unit

    (* Filtering tokens from a list of lexical units *)

    val filter_tokens : 'token t list -> 'token list

    (* Printing *)

    val print :
      token_to_lexeme:('token -> string) ->
      token_to_string:(offsets:bool -> [`Byte | `Point] -> 'token -> string) ->
      Buffer.t ->
      'token t ->
      unit
  end

module Make (Options : Options.S) =
  struct
    module Options = Options

    type 'token lex_unit = [
      `Token     of 'token
    | `Markup    of Markup.t
    | `Directive of Directive.t
    ]

    type 'token t = 'token lex_unit

    let filter_tokens units =
      let filter = function
        `Token t -> Some t
      | _ -> None
      in List.filter_map filter units

    (* Printing *)

    let print ~token_to_lexeme ~token_to_string buffer lex_unit : unit =
      let output    str = Buffer.add_string buffer str in
      let output_nl str = Buffer.add_string buffer (str ^ "\n")
      and offsets       = Options.offsets
      and mode          = Options.mode in
      match Options.command with
        Some `Tokens ->
          (match lex_unit with
             `Token t -> token_to_string ~offsets mode t |> output_nl
           | `Markup _ | `Directive _ -> ()) (* Only tokens *)
      | Some `Copy ->
          let lexeme =
            match lex_unit with
              `Token t     -> token_to_lexeme t
            | `Markup m    -> Markup.to_lexeme m
            | `Directive d -> Directive.to_lexeme d
          in output lexeme
      | Some `Units ->
          let string =
            match lex_unit with
              `Token t     -> token_to_string ~offsets mode t
            | `Markup m    -> Markup.to_string ~offsets mode m
            | `Directive d -> Directive.to_string ~offsets mode d
          in output_nl string
      | None -> ()
  end
