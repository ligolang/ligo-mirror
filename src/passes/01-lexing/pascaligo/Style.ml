(* Checking style of PascaLIGO based on the lexical context *)

(* Vendor dependencies *)

module Core = LexerLib.Core
module Region = Simple_utils.Region

(* Finding the next token in a list of lexical units *)

let rec next_token markup = function
  Core.Token token :: _ ->
    Some (List.rev markup, token)
| Core.Markup m :: units ->
    next_token (m::markup) units
| Core.Directive _ :: units ->
    next_token markup units
| [] -> None

let next_token = next_token []

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

let fail region error =
  let msg = error_to_string error in
  Stdlib.Error Region.{value=msg;region}

(* Predicates on the tokens *)

let is_int    = function Token.Int    _ -> true | _ -> false
let is_string = function Token.String _ -> true | _ -> false
let is_bytes  = function Token.Bytes  _ -> true | _ -> false
let is_eof    = function Token.EOF    _ -> true | _ -> false

let is_hexa = function
  Token.UIdent
    Region.{value="A"|"a"|"B"|"b"|"C"|"c"
                 |"D"|"d"|"E"|"e"|"F"|"f"; _} -> true
| _ -> false

let is_sym =
  let open Token in
  function
    SEMI     _  (* ";"   *)
  | COMMA    _  (* ","   *)
  | LPAR     _  (* "("   *)
  | RPAR     _  (* ")"   *)
  | LBRACE   _  (* "{"   *)
  | RBRACE   _  (* "}"   *)
  | LBRACKET _  (* "["   *)
  | RBRACKET _  (* "]"   *)
  | SHARP    _  (* "#"   *)
  | VBAR     _  (* "|"   *)
  | ARROW    _  (* "->"  *)
  | ASSIGN   _  (* ":="  *)
  | EQ       _  (* "="   *)
  | COLON    _  (* ":"   *)
  | LT       _  (* "<"   *)
  | LE       _  (* "<="  *)
  | GT       _  (* ">"   *)
  | GE       _  (* ">="  *)
  | NE       _  (* "=/=" *)
  | PLUS     _  (* "+"   *)
  | MINUS    _  (* "-"   *)
  | SLASH    _  (* "/"   *)
  | TIMES    _  (* "*"   *)
  | DOT      _  (* "."   *)
  | WILD     _  (* "_"   *)
  | CARET    _  (* "^"   *)
    -> true

  (* Preprocessing directives *)

  | Directive _

  (* Literals *)

  | String   _
  | Verbatim _
  | Bytes    _
  | Int      _
  | Nat      _
  | Mutez    _
  | Ident    _
  | UIdent   _
  | Lang     _
  | Attr     _

  (* Keywords *)

  | And       _  (* and        *)
  | Begin     _  (* begin      *)
  | BigMap    _  (* big_map    *)
  | Block     _  (* block      *)
  | Case      _  (* case       *)
  | Const     _  (* const      *)
  | Contains  _  (* contains   *)
  | Else      _  (* else       *)
  | End       _  (* end        *)
  | For       _  (* for        *)
  | From      _  (* from       *)
  | Function  _  (* function   *)
  | If        _  (* if         *)
  | In        _  (* in         *)
  | Is        _  (* is         *)
  | List      _  (* list       *)
  | Map       _  (* map        *)
  | Mod       _  (* mod        *)
  | Module    _  (* module     *)
  | Nil       _  (* nil        *)
  | Not       _  (* not        *)
  | Of        _  (* of         *)
  | Or        _  (* or         *)
  | Patch     _  (* patch      *)
  | Record    _  (* record     *)
  | Recursive _  (* recursive  *)
  | Remove    _  (* remove     *)
  | Set       _  (* set        *)
  | Skip      _  (* skip       *)
  | Step      _  (* step       *)
  | Then      _  (* then       *)
  | To        _  (* to         *)
  | Type      _  (* type       *)
  | Var       _  (* var        *)
  | While     _  (* while      *)
  | With      _  (* with       *)

  (* End Of File *)

  | EOF _ -> false


(* Checking the style *)

type lex_units = Token.t Core.lex_unit list

type message = string Region.reg

let rec check orig = function
  Core.Token token :: units ->
    let pos    = (Token.to_region token)#stop in
    let region = Region.make ~start:pos ~stop:pos in
    (match next_token units with
       Some ([], next) ->
         let open Token in
         if   is_int token || is_string token
         then if   is_sym next || is_eof next
              then check orig units
              else fail region Missing_break
         else
           if   is_bytes token
           then if   is_int next || is_hexa next
                then fail region Odd_lengthed_bytes
                else
                  if   is_sym next || is_eof next
                  then check orig units
                  else fail region Missing_break
           else check orig units
     | _ -> check orig units)
| (Core.Markup _ | Core.Directive _) :: units ->
     check orig units
| [] -> Ok orig

let check = function
  Stdlib.Ok units -> check units units
| Error _ as err -> err
