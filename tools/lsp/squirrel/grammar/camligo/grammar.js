let sepBy1 = (sep, p) => seq(p, repeat(seq(sep, p)))
let sepBy  = (sep, p) => optional(sepBy1(sep, p))

let some = x => seq(x, repeat(x))

let withAttrs = ($, x) => seq(field("attributes", repeat($.attr)), x)

function mkOp($, opExpr) {
  return seq(
    field("left", $._expr),
    field("op", opExpr),
    field("right", $._expr)
  );
}

module.exports = grammar({
  name: 'CameLigo',
  word:   $ => $.Keyword,
  extras: $ => [$.ocaml_comment, $.comment, /\s/],

  rules: {
    source_file: $ => repeat(field("declaration", $._declaration)),

    _declaration: $ => choice(
      $.let_decl,
      $.fun_decl,
      $.type_decl,
      $.include,
    ),

    include: $ => seq(
      '#include',
      field("filename", $.String)
    ),

    fun_decl: $ => withAttrs($, seq(
      "let",
      optional(field("recursive", "rec")),
      field("name", $.NameDecl),
      some(field("arg", $.annot_pattern)),
      optional(seq(
        ":",
        field("type", $._type_expr)
      )),
      "=",
      field("body",$._program),
    )),

    let_decl: $ => withAttrs($, seq(
      "let",
      optional(field("recursive", "rec")),
      field("name", $._pattern),
      optional(seq(
        ":",
        field("type", $._type_expr)
      )),
      "=",
      field("body",$._program),
    )),

    //========== EXPR ============

    _program: $ => choice(
      $.let_expr1,
      $._expr
    ),

    let_expr1: $ => seq(
      field("decl", $.let_decl),
      "in",
      field("body", $._program)
    ),

    // [1;2]
    list_pattern: $ => seq(
      "[",
      sepBy(';', field("item", $._pattern)),
      "]"
    ),

    // a :: b
    list_con_pattern: $ => prec.right(9, seq(
      field("x", $._pattern),
      "::",
      field("xs", $._pattern)
    )),

    // a, b, c
    tup_pattern: $ => prec.right(8,seq(
      field("item", $._pattern),
      ",",
      sepBy1(",", field("item", $._pattern))
    )),

    _pattern: $ => choice(
      $.var_pattern,
      $._paren_pattern,
      $.con_pattern,
      $._literal,
      $.list_pattern,
      $.list_con_pattern,
      $.tup_pattern,
      "_"
    ),

    var_pattern: $ => seq(
      field("var", $.NameDecl)
    ),

    con_pattern: $ => prec(10,
      seq(
        field("ctor", $.ConstrName),
        optional(field("args",$._pattern))
      )
    ),

    _paren_pattern: $ => choice(
      $.annot_pattern,
      $.paren_pattern,
    ),

    paren_pattern: $ =>
      seq(
        "(",
        field("pat", $._pattern),
        ")"
      ),

    annot_pattern: $ =>
      seq(
        "(",
        field("pat", $._pattern),
        ":",
        field("type", $._type_expr),
        ")"
      ),

    _call: $ => choice(
      $.unary_op_app,
      $.binary_op_app,
    ),

    binary_op_app: $ => choice(
      prec.left(16, mkOp($, "mod")),
      prec.left(15, mkOp($, choice("/", "*"))),
      prec.left(14, mkOp($, choice("-", "+"))),
      prec.right(13, mkOp($, "::")),
      prec.right(12, mkOp($, "^")),
      prec.left(11, mkOp($, choice("&&", "||"))),
      prec.left(10, mkOp($, choice("=", "<>", "==", "<", "<=", ">", ">="))),
    ),

    // - a
    unary_op_app: $ => prec(19, seq(
      field("negate", "-"),
      field("arg", $._expr)
    )),

    // f a
    fun_app: $ => prec.left(20, seq(
      field("f", $._sub_expr),
      field("x", $._sub_expr)
    )),

    _accessor: $ => choice($.FieldName, $.Int),

    // field names (or indices) separated by a dot
    _accessor_chain: $ => prec.right(sepBy1('.', field("accessor", $._accessor))),

    // a.0 or a.attribute
    data_projection: $ => prec.right(21, seq(
      field("box", $._sub_expr),
      ".",
      $._accessor_chain,
    )),

    // { p with a = b; c = d }
    rec_literal: $ => seq(
      "{",
      sepBy1(";", field("field", $.rec_assignment)),
      optional(";"),
      "}"
    ),

    // { p with a = b; c = d }
    rec_expr: $ => seq(
      "{",
      field("subject", $.Name),
      "with",
      field("field", $.rec_assignment),
      repeat(seq(";", field("field", $.rec_assignment))),
      optional(";"),
      "}"
    ),
    // a = b;
    rec_assignment: $ => seq(
      $._accessor_chain,
      "=",
      field("value", $._expr),
    ),

    // if a then b else c
    if_expr: $ => prec.right(seq(
      "if",
      field("condition", $._expr),
      "then",
      field("then", $._program),
      optional(seq(
        "else",
        field("else", $._program)
      ))
    )),

    // match x with ...
    match_expr: $ => prec.right(1,seq(
      "match",
      field("subject", $._expr),
      "with",
      optional('|'),
      sepBy('|', field("alt", $.matching))
    )),

    // Dog as x -> f x
    matching: $ => seq(
      field("pattern", $._pattern),
      "->",
      field("body", $._program)
    ),

    lambda_expr: $ => seq(
      "fun",
      repeat1(field("arg", $.annot_pattern)),
      "->",
      field("body", $._program)
    ),

    list_expr: $ => seq(
      "[",
      sepBy(";", field("item", $._expr)),
      "]"
    ),

    tup_expr: $ => prec.right(9,seq(
      field("x", $._expr),
      some(seq(
        ",",
        field("x", $._expr),
      )),
    )),

    _expr: $ => choice(
      $._call,
      $._sub_expr,
      $.tup_expr
    ),

    _sub_expr: $ => choice(
      $.fun_app,
      $.paren_expr,
      $.annot_expr,
      $.Name,
      $.ConstrName,
      $._literal,
      $.rec_expr,
      $.rec_literal,
      $.if_expr,
      $.lambda_expr,
      $.match_expr,
      $.list_expr,
      $.data_projection,
      $.block_expr,
    ),

    block_expr: $ => seq(
      "begin",
      sepBy(";", field("item", $._program)),
      "end",
    ),

    paren_expr: $ => seq(
      "(",
      field("expr", $._program),
      ")"
    ),

    annot_expr: $ => seq(
      "(",
      field("expr", $._program),
      ":",
      field("type", $._type_expr),
      ")",
    ),

    // a t, (a, b) t
    type_app: $ => prec(10,seq(
      choice(
        field("x", $._type_expr),
        field("x", $.type_tuple),
      ),
      field("f", $.TypeName)
    )),

    type_tuple: $ => seq(
      "(",
      sepBy1(",", field("x", choice($._type_expr, $.String))),
      ")"
    ),

    // string * integer
    type_product: $ => prec.right(5, seq(
      field("x", $._type_expr),
      some(seq(
        "*",
        field("x", $._type_expr)
      ))
    )),

    // int -> string
    type_fun: $ => prec.right(8, seq(
      field("domain", $._type_expr),
      "->",
      field("codomain", $._type_expr)
    )),

    _type_expr: $ => choice(
      $.type_fun,
      $.type_product,
      $.type_app,
      $.TypeName,
      $.type_tuple,
    ),

    // Cat of string, Person of string * string
    variant: $ => withAttrs($, seq(
      field("constructor", $.ConstrName),
      optional(seq(
        "of",
        field("type", $._type_expr)
      ))
    )),

    type_sum: $ => seq(choice(
      sepBy1('|', field("variant", $.variant)),
      withAttrs($, seq('|', sepBy1('|', field("variant", $.variant)))),
    )),

    _label: $ => $.FieldName,

    // field : string * int
    type_rec_field: $ => withAttrs($, seq(
      field("field", $._label),
      ":",
      field("type", $._type_expr)
    )),

    // { field1 : a; field2 : b }
    type_rec: $ => withAttrs($, seq(
      "{",
      sepBy(";", field("field", $.type_rec_field)),
      optional(";"),
      "}"
    )),

    _type_def_body: $ => choice(
      $.type_sum,
      $._type_expr,
      $.type_rec
    ),

    type_decl: $ => seq(
      "type",
      field("name", $.TypeName),
      "=",
      field("type", $._type_def_body)
    ),

    _literal: $ => choice(
      $.String,
      $.Int,
      $.Nat,
      $.Tez,
      $.Bytes,
      $.True,
      $.False,
      $.Unit
    ),

    attr: $ => /\[@[a-zA-Z][a-zA-Z0-9_:]*\]/,

    String:       $ => /\"(\\.|[^"])*\"/,
    Int:          $ => /-?([1-9][0-9_]*|0)/,
    Nat:          $ => /([1-9][0-9_]*|0)n/,
    Tez:          $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Bytes:        $ => /0x[0-9a-fA-F]+/,
    Name:         $ => /[a-z][a-zA-Z0-9_]*/,
    TypeName:     $ => /[a-z][a-zA-Z0-9_]*/,
    NameDecl:     $ => /[a-z][a-zA-Z0-9_]*/,
    FieldName:    $ => /[a-z][a-zA-Z0-9_]*/,
    ConstrName:   $ => /[A-Z][a-zA-Z0-9_]*/,
    Keyword:      $ => /[A-Za-z][a-z]*/,

    False:         $ => 'false',
    True:          $ => 'true',
    Unit:          $ => '()',

    comment: $ => /\/\/(\*\)[^\n]|\*[^\)\n]|[^\*\n])*\n/,

    ocaml_comment: $ =>
      seq(
        '(*',
        repeat(choice(
          $.ocaml_comment,
          /[^\*\(]/,
          /\*[^\)]/,
          /\([^\*]/,
        )),
        /\*+\)/
      ),
  }
});
