/**
 * @file Lune is good
 * @author Gareth Evans <garrydanger@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "lune",

  rules: {
    // TODO: add the actual grammar rules
    source_file: $ => "hello"
  }
});
