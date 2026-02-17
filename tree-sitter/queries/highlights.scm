; Lune Tree-sitter Highlights
; Designed for Elm-like syntax highlighting quality

; ========== COMMENTS ==========
(line_comment) @comment.line
(block_comment) @comment.block

; ========== KEYWORDS ==========
[
  "module"
  "exposing"
  "import"
  "as"
  "type"
  "alias"
  "newtype"
  "class"
  "instance"
  "where"
  "let"
  "in"
  "case"
  "of"
  "if"
  "then"
  "else"
  "do"
  "foreign"
] @keyword

; ========== LITERALS ==========
(integer_literal) @number
(float_literal) @number.float
(char_literal) @character
(string_literal) @string
(multiline_string) @string
(string_content) @string
(escape_sequence) @string.escape

; ========== INTERPOLATION ==========
(interpolation
  "${" @punctuation.special
  "}" @punctuation.special)
(interpolation
  expression: (_) @embedded)

; ========== TYPES ==========
(type_identifier) @type
(type_variable) @type.parameter

; Type constructors in type declarations
(constructor_definition
  name: (constructor_identifier) @type.constructor)

; Constructor identifiers in expressions/patterns
(constructor_identifier) @constructor

; ========== FUNCTIONS AND VARIABLES ==========
; Function name in declaration
(function_declaration
  name: (identifier) @function.definition)

; Function name in type signature
(type_signature
  name: (identifier) @function)

; Method signatures in classes
(method_signature
  name: (identifier) @function.method)

; Method definitions in instances
(method_definition
  name: (identifier) @function.method)

; Function application
(function_application
  function: (identifier) @function.call)

; Regular identifiers (variables)
(identifier) @variable

; ========== PATTERNS ==========
(wildcard_pattern) @variable.builtin
(variable_pattern
  (identifier) @variable.parameter)

; ========== OPERATORS ==========
(operator) @operator

; Operator identifiers (in parentheses for prefix notation)
(operator_identifier
  "(" @punctuation.bracket
  (operator) @operator
  ")" @punctuation.bracket)

; Binary expression operators
(binary_expression
  operator: (operator) @operator)

; ========== MODULES ==========
(module_declaration
  name: (module_identifier) @module)

(import_declaration
  module: (module_identifier) @module)

(import_declaration
  alias: (type_identifier) @module)

; Qualified identifiers
(qualified_identifier
  module: (module_path) @module)

; ========== ANNOTATIONS ==========
(annotation
  "@" @attribute.builtin
  name: (identifier) @attribute)

(field_annotation
  "@" @attribute.builtin
  name: (identifier) @attribute)

; ========== SPECIAL KEYWORDS ==========
; Calling convention in FFI
(calling_convention) @keyword.function

; Arrow types
"->" @operator

; Constraint arrow
"=>" @operator

; Pipe operators
"|" @operator

; ========== PUNCTUATION ==========
["(" ")" "[" "]" "{" "}"] @punctuation.bracket
["," ":"] @punctuation.delimiter
"." @punctuation.delimiter
"=" @operator
"\\" @operator
"<-" @operator
"|>" @operator

; ========== RECORDS ==========
(record_field
  name: (identifier) @property)

(record_type_field
  name: (identifier) @property)

(record_pattern_field
  name: (identifier) @property)

(field_access_expression
  field: (identifier) @property)

; ========== CLASS AND INSTANCE ==========
(class_declaration
  name: (type_identifier) @type.class)

(instance_declaration
  class: (type_identifier) @type.class)

(constraint
  class: (type_identifier) @type.class)

; ========== EXPOSE ALL ==========
(expose_all) @punctuation.special

; ========== ERROR ==========
(ERROR) @error
