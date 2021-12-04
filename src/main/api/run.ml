open Api_helpers
open Simple_utils
module Compile = Ligo_compile
module Helpers   = Ligo_compile.Helpers
module Run = Ligo_run.Of_michelson

let test source_file syntax steps infer protocol_version display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Ligo_interpreter.Formatter.tests_format) get_warnings @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
      let options = Compiler_options.make ~infer ~test:true ~protocol_version () in
      let typed,_ = Build.build_context ~raise ~add_warning ~options syntax source_file in
      let aggregated = Ligo_compile.Of_typed.compile_program ~raise typed in
      Interpreter.eval_test ~raise ~steps ~protocol_version typed aggregated

let dry_run source_file entry_point input storage amount balance sender source now syntax infer protocol_version display_format werror =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~werror ~display_format (Decompile.Formatter.expression_format) get_warnings @@
      fun ~raise ->
      let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
      let options = Compiler_options.make ~infer ~protocol_version () in
      let typed_prg,env = Build.build_context ~raise ~add_warning ~options syntax source_file in
      let aggregated_prg = Compile.Of_typed.apply_to_entrypoint ~raise (typed_prg,env) entry_point in
      let mini_c_prg = Compile.Of_aggregated.compile_expression ~raise aggregated_prg in
      let compile_exp = Compile.Of_mini_c.compile_contract ~raise ~options mini_c_prg in
      let parameter_ty =
        (* fails if the given entry point is not a valid contract *)
        let _contract = Compile.Of_michelson.build_contract ~raise compile_exp in
        Option.map ~f:fst @@ Self_michelson.fetch_contract_ty_inputs compile_exp.expr_ty
      in
      let compiled_params   = Compile.Utils.compile_contract_input ~raise ~options input storage source_file syntax env in
      let args_michelson    = Run.evaluate_expression ~raise compiled_params.expr compiled_params.expr_ty in

      let options           = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty } in
      let runres  = Run.run_contract ~raise ~options compile_exp.expr compile_exp.expr_ty args_michelson in
      Decompile.Of_michelson.decompile_value_from_contract_execution ~raise aggregated_prg.type_expression runres

let interpret expression init_file syntax infer protocol_version amount balance sender source now display_format =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~display_format (Decompile.Formatter.expression_format) get_warnings @@
      fun ~raise ->
      let options =
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        Compiler_options.make ~infer ~protocol_version ()
      in
      let (mini_c_exp, typed_exp) = Build.build_expression ~raise ~add_warning ~options syntax expression init_file in
      let compiled_exp = Compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp in
      let options           = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
      let runres  = Run.run_expression ~raise ~options compiled_exp.expr compiled_exp.expr_ty in
      Decompile.Of_michelson.decompile_expression ~raise typed_exp.type_expression runres

let evaluate_call source_file entry_point parameter amount balance sender source now syntax infer protocol_version display_format werror =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~werror ~display_format (Decompile.Formatter.expression_format) get_warnings @@
      fun ~raise ->
      let options =
        let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
        Compiler_options.make ~infer ~protocol_version ()
      in
      let _,env = Build.build_context ~raise ~add_warning ~options syntax source_file in
      let meta             = Compile.Of_source.extract_meta ~raise syntax source_file in
      let c_unit_param,_   = Compile.Of_source.compile_string ~raise ~options ~meta parameter in
      let imperative_param = Compile.Of_c_unit.compile_expression ~raise ~meta c_unit_param in
      let sugar_param      = Compile.Of_imperative.compile_expression ~raise imperative_param in
      let core_param       = Compile.Of_sugar.compile_expression sugar_param in
      let app              = Compile.Of_core.apply entry_point core_param in
      let typed_app,_      = Compile.Of_core.compile_expression ~raise ~options ~env app in
      let app_aggregated   = Compile.Of_typed.compile_expression ~raise typed_app in
      let app_mini_c       = Compile.Of_aggregated.compile_expression ~raise app_aggregated in
      let michelson        = Compile.Of_mini_c.compile_expression ~raise ~options app_mini_c in
      let options          = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None} in
      let runres           = Run.run_expression ~raise ~options michelson.expr michelson.expr_ty in
      Decompile.Of_michelson.decompile_expression ~raise app_aggregated.type_expression runres

let evaluate_expr init_file expression amount balance sender source now syntax infer protocol_version display_format werror =
    Trace.warning_with @@ fun add_warning get_warnings ->
    format_result ~werror ~display_format Decompile.Formatter.expression_format get_warnings @@
      fun ~raise ->
        let options =
          let protocol_version = Helpers.protocol_to_variant ~raise protocol_version in
          Compiler_options.make ~infer ~protocol_version ()
        in
        let (mini_c_exp, typed_exp) = Build.build_expression ~raise ~add_warning ~options syntax expression init_file in
        let compiled_exp = Compile.Of_mini_c.compile_expression ~raise ~options mini_c_exp in
        let options           = Run.make_dry_run_options ~raise {now ; amount ; balance ; sender ; source ; parameter_ty = None } in
        let runres  = Run.run_expression ~raise ~options compiled_exp.expr compiled_exp.expr_ty in
        Decompile.Of_michelson.decompile_expression ~raise typed_exp.type_expression runres
