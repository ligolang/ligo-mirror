(* Parsing command-line options for the lexer *)

(* Vendor dependencies *)

module Argv   = Simple_utils.Argv
module Getopt = GetoptLib.Getopt

(* Lexer parameters *)

module type PARAMETERS =
  sig
    module Config  : Preprocessor.Config.S
    module Options : Options.S
    module Status  : module type of Status
  end

(* Parsing the command line options *)

module Make (PreprocParams: Preprocessor.CLI.PARAMETERS) : PARAMETERS =
  struct
    (* Auxiliary functions and modules *)

    let sprintf = Printf.sprintf

    (* Help (exported) *)

    let make_help buffer : Buffer.t =
      let options = [
        "  -t, --tokens     Print tokens";
        "  -u, --units      Print lexical units";
        "  -c, --copy       Print lexemes and markup";
        "      --bytes      Bytes for source locations";
        "      --pre        Run the preprocessor";
        "      --post=<pass> Run postprocessing up to pass <pass> \
                             (none/0, 1, 2, ..., all)"
      ] in
      begin
        Buffer.add_string buffer (String.concat "\n" options);
        Buffer.add_char   buffer '\n';
        buffer
      end

    (* Global references for the CLI options *)

    type post_pass = Pass of int | All

    let copy       = ref false
    and tokens     = ref false
    and units      = ref false
    and bytes      = ref false
    and pre        = ref false
    and post       = ref (Some All : post_pass option)

    and help       = ref false
    and version    = ref false
    and cli        = ref false

    (* --post *)

    let print_post = function
      None        -> "none"
    | Some Pass n -> string_of_int n
    | Some All    -> "all"

    let post_arg arg =
      if !post = None then
        if arg = "none" then ()
        else if arg = "all" then post := Some All
        else match Stdlib.int_of_string_opt arg with
               None -> raise (Getopt.Error "Invalid pass number.")
             | Some 0 -> post := None
             | Some num when num < 0 ->
                 raise (Getopt.Error "Invalid pass number.")
             | Some num -> post := Some (Pass num)
      else raise (Getopt.Error "Only one --post option allowed.")

    (* Specifying the command-line options a la GNU

       See [GetoptLib.Getopt] for the layout of the command line and
       the specification of the options. *)

    let specs =
      Getopt.[
        noshort, "copy",       set copy true, None;
        noshort, "tokens",     set tokens true, None;
        noshort, "units",      set units true, None;
        noshort, "bytes",      set bytes true, None;
        noshort, "pre",        set pre true, None;
        noshort, "post",       None, Some post_arg;
        noshort, "cli",        set cli true, None;
        'h',     "help",       set help true, None;
        'v',     "version",    set version true, None
      ]

     (* Handler of anonymous arguments: those have been handled by a
        previous IO *)

    let anonymous _arg = ()

    (* Parsing the command-line options *)

    (* We do not want the exception [Getopt.Error] to be raised when
       finding an unknown option.

       The following is a hack to filter out unknown options but
       leaving correct ones, even if their syntax is invalid (this
       will result in an error in [Getopt.parse_cmdline] below. Also,
       we assume that there are no concatenated short options (here,
       the only possible combinations are "-hv" and "-vh") and that
       anonymous arguments (here, a unique text file) is given after
       "--".

       We make a copy of [Sys.argv], we filter it in a list, the
       resulting list is copied to [Sys.argv] (with the remaning cells
       set to [""]), we parse the options with [Getopt.parse_cmdline]
       and we finally restore [Sys.argv] from its original copy. *)

    module SSet = Set.Make (String)

    let opt_wo_arg =
      let open SSet in
      empty
      |> add "--copy"
      |> add "--tokens"
      |> add "--units"
      |> add "--pre"

      (* The following options are present in all CLIs *)
      |> add "--cli"
      |> add "--help"    |> add "-h"
      |> add "--version" |> add "-v"

    let opt_with_arg =
      let open SSet in
      empty
      |> add "--post"

    let argv_copy = Array.copy Sys.argv

    let () = Argv.filter ~opt_wo_arg ~opt_with_arg

    type status = [
      PreprocParams.Status.t
    | `Conflict of string * string
    ]

    let status = (PreprocParams.Status.status :> status)

    let status =
      try
        Getopt.parse_cmdline specs anonymous; status
      with Getopt.Error msg -> `SyntaxError msg

    let () =
      for i = 0 to Array.length Sys.argv - 1 do
        Sys.argv.(i) <- argv_copy.(i)
      done

    (* Re-exporting immutable fields with their CLI value *)

    let copy        = !copy
    and tokens      = !tokens
    and units       = !units
    and mode        = if !bytes then `Byte else `Point
    and preprocess  = !pre
    and postprocess = !post

    (* Re-exporting and printing on stdout the CLI options *)

    let make_cli buffer : Buffer.t =
      (* Options "help", "version" and "cli" are not given. *)
      let options = [
        sprintf "copy   = %b" copy;
        sprintf "tokens = %b" tokens;
        sprintf "units  = %b" units;
        sprintf "bytes  = %b" !bytes;
        sprintf "pre    = %b" preprocess;
        sprintf "post   = %s" (print_post postprocess)] in
    begin
      Buffer.add_string buffer (String.concat "\n" options);
      Buffer.add_char   buffer '\n';
      buffer
    end

    (* STATUS *)

    (* Checking combinations of options *)

    let status, command =
      match copy, units, tokens with
        true, false, false -> status, Some `Copy
      | false,  true, false -> status, Some `Units
      | false, false,  true -> status, Some `Tokens
      | false, false, false -> status, None
      | true, true, _ -> `Conflict ("--copy", "--units"), None
      | true, _, true -> `Conflict ("--copy", "--tokens"), None
      | _, true, true -> `Conflict ("--units", "--tokens"), None

    let status =
      match status with
        `Help buffer  -> `Help (make_help buffer)
      | `CLI buffer   -> `CLI (make_cli buffer)
      | `Version _    -> `Version Version.version
      | _             -> status

    (* Packaging *)

    module Config = PreprocParams.Config

    module Options =
      struct
        include PreprocParams.Options

        type nonrec post_pass = post_pass = Pass of int | All

        let postprocess = postprocess
        let preprocess  = preprocess
        let mode        = mode
        let command     = command
      end

    module Status =
      struct
        type t = status
        type nonrec status = status
        let status = status
      end
  end
