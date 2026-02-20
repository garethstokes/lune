/**
 * @file Tree-sitter grammar for the Lune programming language
 * @author Gareth Stokes <garrydanger@gmail.com>
 * @license MIT
 *
 * Lune is a Haskell-inspired functional language with:
 * - ML-style module system with exposing lists
 * - Algebraic data types and type aliases
 * - Typeclasses and instances
 * - Do-notation for monadic sequencing
 * - Record types with field access
 * - Template literals (inline and multiline)
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  COMMENT: 0,
  ASSIGN: 1,
  BACKWARD_PIPE: 2,
  FORWARD_PIPE: 3,
  ARROW: 4,
  CONCAT: 6,
  COMPARE: 5,
  ADD: 7,
  MULT: 8,
  CALL: 10,
  FIELD: 11,
};

module.exports = grammar({
  name: "lune",

  // External tokens - order MUST match enum in scanner.c
  externals: $ => [
    $._virtual_end_decl,    // Separates items at same indent level
    $._virtual_open_section, // Opens a layout block
    $._virtual_end_section,  // Closes a layout block
    $._identifier_ext,       // Identifier via external scanner
    $._constructor_ext,      // Constructor via external scanner
    $._type_variable_ext,    // Type variable via external scanner
  ],

  extras: $ => [
    /\s/,
    $.line_comment,
    $.block_comment,
  ],

  word: $ => $._identifier_token,

  inline: $ => [
    $._declaration,
    $._expression,
    $._type,
    $._pattern,
  ],

  conflicts: $ => [
    // In do-blocks, patterns and expressions can start the same way
    [$.list_pattern, $.list_expression],
    [$.record_pattern, $.record_expression],
    // type_identifier and constructor_identifier share the same token
    [$.type_identifier, $.constructor_identifier],
  ],

  rules: {
    source_file: $ => seq(
      $.module_declaration,
      repeat($.import_declaration),
      repeat($._declaration),
    ),

    // ========== MODULE DECLARATION ==========
    module_declaration: $ => seq(
      'module',
      field('name', $.module_identifier),
      field('exports', $.exposing_list),
    ),

    // Module identifier: composed of type_identifiers joined by dots
    // We parse it as a sequence rather than a regex to avoid token conflicts
    module_identifier: $ => prec.right(seq(
      $._module_segment,
      repeat(seq('.', $._module_segment)),
    )),

    // Module segment: reuse type_identifier token to avoid conflicts
    _module_segment: $ => $.type_identifier,

    exposing_list: $ => seq(
      'exposing',
      '(',
      sepBy(',', $.exposing_item),
      ')',
    ),

    exposing_item: $ => choice(
      seq(
        field('name', $.type_identifier),
        optional($.expose_all),
      ),
      field('name', $.identifier),
      field('name', $.operator_identifier),
    ),

    expose_all: $ => '(..)',

    // ========== IMPORTS ==========
    import_declaration: $ => seq(
      'import',
      field('module', $.module_identifier),
      optional(seq('as', field('alias', $.type_identifier))),
      optional(field('exposing', $.exposing_list)),
    ),

    // ========== DECLARATIONS ==========
    _declaration: $ => choice(
      $.type_declaration,
      $.type_alias_declaration,
      $.newtype_declaration,
      $.class_declaration,
      $.instance_declaration,
      $.foreign_import_declaration,
      $.type_signature,
      $.function_declaration,
      $.annotated_declaration,
    ),

    // Annotation (@derive, @primaryKey, etc.)
    // Use token.immediate to ensure ( is parsed with the annotation
    annotation: $ => seq(
      '@',
      field('name', $.identifier),
      optional(seq(
        token.immediate('('),
        field('arguments', $._annotation_arguments),
        ')',
      )),
    ),

    _annotation_arguments: $ => repeat1($._annotation_atom),

    // Annotation arguments - use constructor_identifier to avoid module_identifier conflict
    _annotation_atom: $ => choice(
      $.constructor_identifier,
      $.string_literal,
      $.integer_literal,
      $.identifier,
    ),

    annotated_declaration: $ => seq(
      repeat1($.annotation),
      choice(
        $.type_alias_declaration,
        $.function_declaration,
      ),
    ),

    // Type declaration: type Bool = True | False
    type_declaration: $ => seq(
      'type',
      field('name', $.type_identifier),
      field('parameters', repeat($.type_variable)),
      '=',
      field('constructors', $.constructor_list),
    ),

    constructor_list: $ => sepBy1('|', $.constructor_definition),

    // Constructor in ADT: name followed by type parameters
    // Use prec.right to consume all following type atoms greedily
    constructor_definition: $ => prec.right(PREC.CALL + 1, seq(
      field('name', $.constructor_identifier),
      field('parameters', repeat($._type_atom)),
    )),

    // Type alias: type alias Config = { port : Int, host : String }
    type_alias_declaration: $ => seq(
      'type',
      'alias',
      field('name', $.type_identifier),
      field('parameters', repeat($.type_variable)),
      '=',
      field('type', $._type),
    ),

    // Newtype: newtype Parser a = Parser (String -> Maybe (a, String))
    newtype_declaration: $ => seq(
      'newtype',
      field('name', $.type_identifier),
      field('parameters', repeat($.type_variable)),
      '=',
      field('constructor', $.constructor_identifier),
      field('type', $._type),
    ),

    // Class declaration - right assoc to consume all methods
    class_declaration: $ => prec.right(seq(
      'class',
      optional(seq(
        field('superclasses', $.constraint_list),
        '=>',
      )),
      field('name', choice($.type_identifier, $.qualified_type_identifier)),
      field('parameters', repeat1($.class_parameter)),
      'where',
      field('methods', repeat1($.method_signature)),
    )),

    class_parameter: $ => prec(2, choice(
      $.type_variable,
      seq('(', $.type_variable, ':', $._kind, ')'),
    )),

    _kind: $ => prec.right(sepBy1('->', $._kind_atom)),

    _kind_atom: $ => choice(
      'Type',
      seq('(', $._kind, ')'),
    ),

    method_signature: $ => seq(
      field('name', $.identifier),
      ':',
      field('type', $.qualified_type),
    ),

    // Instance declaration - right assoc to consume all methods
    instance_declaration: $ => prec.right(seq(
      'instance',
      field('class', choice($.type_identifier, $.qualified_type_identifier)),
      field('type', $.instance_type),
      'where',
      field('methods', repeat1($.method_definition)),
    )),

    instance_type: $ => repeat1($._type_atom),

    method_definition: $ => seq(
      field('name', $.identifier),
      '=',
      field('body', $._expression),
    ),

    // Foreign import
    foreign_import_declaration: $ => seq(
      'foreign',
      'import',
      $.calling_convention,
      field('symbol', $.string_literal),
      field('name', $.identifier),
      ':',
      field('type', $.qualified_type),
    ),

    calling_convention: $ => 'ccall',

    // Type signature: main : Task Unit Unit
    type_signature: $ => seq(
      field('name', $._function_name),
      ':',
      field('type', $.qualified_type),
    ),

    // Function/value declaration: main = do ...
    function_declaration: $ => seq(
      field('name', $._function_name),
      field('parameters', repeat($._pattern_atom)),
      '=',
      field('body', $._expression),
    ),

    _function_name: $ => choice(
      $.identifier,
      $.operator_identifier,
    ),

    operator_identifier: $ => seq(
      '(',
      $.operator,
      ')',
    ),

    // ========== TYPES ==========
    // Qualified types: optionally constrained
    // e.g. "Show a => a -> String" or just "Int -> String"
    qualified_type: $ => seq(
      optional(seq($.constraint_list, '=>')),
      $._type,
    ),

    constraint_list: $ => choice(
      $.constraint,
      seq('(', sepBy1(',', $.constraint), ')'),
    ),

    // A constraint is a class applied to type arguments
    // e.g. "Show a" or "Functor f"
    constraint: $ => prec(3, seq(
      field('class', choice($.type_identifier, $.qualified_type_identifier)),
      field('arguments', repeat1($.type_variable)),
    )),

    _type: $ => $.function_type,

    function_type: $ => prec.right(PREC.ARROW, sepBy1('->', $.type_application)),

    // Type application: binary left-associative like Haskell tree-sitter
    // f a b parses as ((f a) b) rather than f (a, b)
    type_application: $ => choice(
      prec.left(PREC.CALL, seq(
        field('constructor', $.type_application),
        field('argument', $._type_atom),
      )),
      $._type_atom,
    ),

    _type_atom: $ => choice(
      $.qualified_type_identifier,
      $.type_identifier,
      $.type_variable,
      $.record_type,
      $.parenthesized_type,
      $.tuple_type,
      $.unit_type,
    ),

    qualified_type_identifier: $ => prec.left(seq(
      field('module', choice($.type_identifier, $.qualified_type_identifier)),
      '.',
      field('name', $.type_identifier),
    )),

    parenthesized_type: $ => seq('(', $._type, ')'),

    unit_type: $ => seq('(', ')'),

    tuple_type: $ => seq(
      '(',
      $._type,
      repeat1(seq(',', $._type)),
      ')',
    ),

    record_type: $ => seq(
      '{',
      sepBy(',', $.record_type_field),
      '}',
    ),

    record_type_field: $ => seq(
      field('name', $.identifier),
      ':',
      field('type', $._type),
      repeat($.field_annotation),
    ),

    field_annotation: $ => seq(
      '@',
      field('name', $.identifier),
      optional(seq(
        '(',
        $._annotation_arguments,
        ')',
      )),
    ),

    // Uppercase identifier: used for type names and constructor names
    // Both share the same token - they're semantically distinguished by context
    _upper_identifier: $ => /[A-Z][a-zA-Z0-9_#]*/,
    type_identifier: $ => alias($._upper_identifier, $.type_identifier),
    // Type variables use external scanner to avoid consuming function declarations
    type_variable: $ => $._type_variable_ext,
    constructor_identifier: $ => alias($._upper_identifier, $.constructor_identifier),

    // ========== EXPRESSIONS ==========
    _expression: $ => choice(
      $.let_expression,
      $.case_expression,
      $.lambda_expression,
      $.do_expression,
      $.if_expression,
      $._binary_expression,
    ),

    _binary_expression: $ => choice(
      $.backward_pipe_expression,
      $.forward_pipe_expression,
      $.binary_expression,
      $._application_expression,
    ),

    binary_expression: $ => prec.left(PREC.CONCAT, seq(
      field('left', $._application_expression),
      field('operator', $.operator),
      field('right', $._binary_expression),
    )),

    backward_pipe_expression: $ => prec.right(PREC.BACKWARD_PIPE, seq(
      field('function', $._binary_expression),
      '<|',
      field('argument', $._binary_expression),
    )),

    forward_pipe_expression: $ => prec.left(PREC.FORWARD_PIPE, seq(
      field('left', $._binary_expression),
      '|>',
      field('right', $._binary_expression),
    )),

    _application_expression: $ => choice(
      $.function_application,
      $._accessor_expression,
    ),

    // Function application is left-associative: `f x y` parses as `((f x) y)`.
    // Using left recursion avoids reducing too early on `(`, which would
    // otherwise be misinterpreted as starting a new top-level declaration.
    function_application: $ => prec.left(PREC.CALL, seq(
      field('function', $._application_expression),
      field('arguments', $._accessor_expression),
    )),

    _accessor_expression: $ => choice(
      $.field_access_expression,
      $._primary_expression,
    ),

    field_access_expression: $ => prec.left(PREC.FIELD, seq(
      field('value', $._primary_expression),
      repeat1(seq('.', field('field', $.identifier))),
    )),

    _primary_expression: $ => choice(
      $._expr_identifier,
      $.qualified_identifier,
      $._expr_constructor,
      $.integer_literal,
      $.float_literal,
      $.char_literal,
      $.string_literal,
      $.multiline_string,
      $.list_expression,
      $.record_expression,
      $.record_update_expression,
      $.parenthesized_expression,
      $.tuple_expression,
      $.unit_expression,
      $.operator_section,
    ),

    // Identifier in expression context (via external scanner)
    // Alias to 'identifier' so it shows as a named node in the parse tree
    _expr_identifier: $ => alias($._identifier_ext, $.identifier),

    // Constructor in expression context (via external scanner)
    _expr_constructor: $ => alias($._constructor_ext, $.constructor_identifier),

    qualified_identifier: $ => seq(
      field('module', alias($.module_identifier, $.module_path)),
      '.',
      field('name', choice($.identifier, $.constructor_identifier)),
    ),

    parenthesized_expression: $ => seq('(', $._expression, ')'),

    tuple_expression: $ => seq(
      '(',
      $._expression,
      repeat1(seq(',', $._expression)),
      ')',
    ),

    unit_expression: $ => seq('(', ')'),

    operator_section: $ => seq(
      '(',
      $.operator,
      ')',
    ),

    // Let expression
    let_expression: $ => seq(
      'let',
      repeat1($.let_binding),
      'in',
      field('body', $._expression),
    ),

    let_binding: $ => seq(
      field('name', $.identifier),
      '=',
      field('value', $._expression),
    ),

    // Case expression - uses virtual tokens to separate alternatives
    case_expression: $ => prec.right(seq(
      'case',
      field('scrutinee', $._expression),
      'of',
      $._virtual_open_section,  // Opens layout block
      field('alternatives', $.case_alternative),
      field('alternatives', repeat(seq($._virtual_end_decl, $.case_alternative))),
      optional($._virtual_end_section),
    )),

    case_alternative: $ => seq(
      field('pattern', $._pattern),
      '->',
      field('body', $._expression),
    ),

    // Lambda expression
    lambda_expression: $ => seq(
      '\\',
      field('parameters', repeat1($._pattern_atom)),
      '->',
      field('body', $._expression),
    ),

    // Do expression - uses virtual tokens to separate statements
    do_expression: $ => prec.right(seq(
      'do',
      $._virtual_open_section,  // Opens layout block
      field('statements', $._statement),
      field('statements', repeat(seq($._virtual_end_decl, $._statement))),
      optional($._virtual_end_section),
    )),

    _statement: $ => choice(
      $.bind_statement,
      $.let_statement,
      $.expression_statement,
    ),

    bind_statement: $ => seq(
      field('pattern', $._pattern),
      '<-',
      field('value', $._expression),
    ),

    // In do-blocks, let without 'in' is a statement (higher precedence)
    let_statement: $ => prec(1, seq(
      'let',
      field('name', $.identifier),
      '=',
      field('value', $._expression),
    )),

    expression_statement: $ => $._expression,

    // If expression
    if_expression: $ => seq(
      'if',
      field('condition', $._expression),
      'then',
      field('consequence', $._expression),
      'else',
      field('alternative', $._expression),
    ),

    // List expression
    list_expression: $ => seq(
      '[',
      sepBy(',', $._expression),
      ']',
    ),

    // Record expression
    record_expression: $ => seq(
      '{',
      sepBy(',', $.record_field),
      '}',
    ),

    record_field: $ => seq(
      field('name', $.identifier),
      '=',
      field('value', $._expression),
    ),

    // Record update: { base | field = value }
    record_update_expression: $ => seq(
      '{',
      field('base', $.identifier),
      '|',
      sepBy1(',', $.record_field),
      '}',
    ),

    // ========== PATTERNS ==========
    _pattern: $ => choice(
      $.constructor_pattern,
      $._pattern_atom,
    ),

    // Constructor pattern with arguments needs lower precedence than function application
    // so expressions win in ambiguous contexts (like do-blocks before seeing <-)
    constructor_pattern: $ => prec.left(-1, seq(
      field('constructor', $.constructor_identifier),
      field('arguments', repeat1($._pattern_atom)),
    )),

    _pattern_atom: $ => choice(
      $.wildcard_pattern,
      $.variable_pattern,
      $.literal_pattern,
      $.list_pattern,
      $.tuple_pattern,
      $.parenthesized_pattern,
      $.record_pattern,
      $.nullary_constructor_pattern,
    ),

    // Nullary constructor pattern - needs lower precedence than expressions
    nullary_constructor_pattern: $ => prec(-2, $.constructor_identifier),

    wildcard_pattern: $ => '_',

    // Use external scanner identifier for patterns too (same as expressions)
    // Lower precedence than expressions
    variable_pattern: $ => prec(-2, alias($._identifier_ext, $.identifier)),

    // Lower precedence than expressions so they win in ambiguous contexts
    literal_pattern: $ => prec(-1, choice(
      $.integer_literal,
      $.float_literal,
      $.char_literal,
      $.string_literal,
    )),

    list_pattern: $ => seq(
      '[',
      sepBy(',', $._pattern),
      ']',
    ),

    tuple_pattern: $ => seq(
      '(',
      $._pattern,
      repeat1(seq(',', $._pattern)),
      ')',
    ),

    parenthesized_pattern: $ => seq('(', $._pattern, ')'),

    record_pattern: $ => seq(
      '{',
      sepBy(',', $.record_pattern_field),
      '}',
    ),

    record_pattern_field: $ => seq(
      field('name', $.identifier),
      optional(seq('=', field('pattern', $._pattern))),
    ),

    // ========== LITERALS ==========
    // Internal identifier token for non-expression contexts
    identifier: $ => $._identifier_token,

    // The word token - this must be an internal regex for tree-sitter keyword handling
    _identifier_token: $ => /[a-z_][a-zA-Z0-9_]*/,

    integer_literal: $ => choice(
      /0[xX][0-9a-fA-F]+/,
      /0[oO][0-7]+/,
      /0[bB][01]+/,
      /[0-9]+/,
    ),

    float_literal: $ => /[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?/,

    char_literal: $ => seq(
      "'",
      choice(
        $.escape_sequence,
        token.immediate(/[^'\\]/),
      ),
      token.immediate("'"),
    ),

    // String literal - supports escape sequences and interpolation
    // Both "hello" and "hello ${name}" use the same rule
    string_literal: $ => seq(
      '"',
      repeat(choice(
        $.escape_sequence,
        alias($._string_content, $.string_content),
        $.interpolation,
      )),
      '"',
    ),

    // Content inside strings - avoid ", \, and $ (start of interpolation)
    _string_content: $ => token.immediate(prec(1, /[^"\\$]+/)),

    interpolation: $ => seq(
      '${',
      field('expression', $._expression),
      '}',
    ),

    // Multiline template strings: '' ... ''
    multiline_string: $ => seq(
      "''",
      repeat(choice(
        alias($._multiline_content, $.string_content),
        $.interpolation,
      )),
      "''",
    ),

    // Content in multiline strings - avoid $ (interpolation) and '' (delimiter)
    _multiline_content: $ => token.immediate(prec(1, /[^$']+/)),

    escape_sequence: $ => token.immediate(seq(
      '\\',
      choice(
        /[nrt\\'"0abfv]/,
        /x[0-9a-fA-F]{2}/,
        /u\{[0-9a-fA-F]+\}/,
        '${',  // Escaped interpolation
      ),
    )),

    // ========== OPERATORS ==========
    operator: $ => choice(
      '<>',
      '++',
      '&&',
      '||',
      '==',
      '/=',
      '<=',
      '>=',
      '<',
      '>',
      '+',
      '-',
      '*',
      '/',
      '^',
      '|>',
      '<|',
      '>>',
      '<<',
      '$',
    ),

    // ========== COMMENTS ==========
    line_comment: $ => token(seq('--', /[^\n]*/)),

    // Block comments with nesting support
    // Tree-sitter doesn't support true nested comments in pure grammar,
    // so we use a simplified approach that works for most cases
    block_comment: $ => seq(
      '{-',
      repeat(choice(
        $._block_comment_char,
        $.block_comment,  // Nested comments
      )),
      '-}',
    ),

    // Characters that are safe inside block comments
    // Avoids matching the start/end delimiters
    _block_comment_char: $ => token.immediate(/[^{}-]|[-{}]/),
  },
});

// Helper function for separated lists
function sepBy(sep, rule) {
  return optional(sepBy1(sep, rule));
}

function sepBy1(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)));
}
