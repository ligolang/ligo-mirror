(* Checking style of PascaLIGO based on the lexical context *)

(* Vendor dependencies *)

module Region = Simple_utils.Region
module Unit   = LexerLib.Unit

(* LIGO dependencies *)

module type TOKEN = Lexing_shared.Token.S

(* We need a functor with a module parameter [Token], even though
   [Token] is available in the current directory. The reason is that
   Self_units.mli requires a module parameter [Token] because it is
   located in the directory "shared". Unfortunately, this implies that
   all the nanopasses in this directory need to be functors over
   [Token]. *)

module Make (Token : TOKEN) =
  struct
    (* Finding the next token in a list of lexical units *)

    let rec next_token markup = function
      `Token token :: _     -> Some (List.rev markup, token)
    | `Markup m    :: units -> next_token (m::markup) units
    | `Directive _ :: units -> next_token markup units
    | []                    -> None

    let next_token units = next_token [] units;;
1
    (* Errors *)

    type error =
      Odd_lengthed_bytes
    | Missing_break

    let error_to_string = function
      Odd_lengthed_bytes ->
        "The length of the byte sequence is an odd number.\n\
         Hint: Add or remove a digit."
    | Missing_break ->
        "Missing break.\n\
         Hint: Insert some space."

    let fail acc region error =
      let units = List.rev acc
      and msg = error_to_string error in
      Stdlib.Error (units, Region.{value=msg; region})

    (* Checking the style *)

    type units = Token.t Unit.t list

    type message = string Region.reg

    let rec filter acc = function
      [] -> Ok (List.rev acc)
    | (`Markup _ | `Directive _ as u) :: units ->
         filter (u::acc) units
    | `Token token as t :: units ->
         let pos    = (Token.to_region token)#stop in
         let region = Region.make ~start:pos ~stop:pos in
         match next_token units with
           Some ([], next) ->
             let open Token in
             if   is_int token || is_string token
             then if   is_sym next || is_eof next
                  then filter (t::acc) units
                  else fail acc region Missing_break
             else
               if   is_bytes token
               then if   is_int next || is_hex next
                    then fail acc region Odd_lengthed_bytes
                    else
                      if   is_sym next || is_eof next
                      then filter (t::acc) units
                      else fail acc region Missing_break
               else filter (t::acc) units
         | _ -> filter (t::acc) units

    let filter units = filter [] units
  end
