/**
 * External scanner for Lune - Elm-style indentation handling
 *
 * Based on the elm-tooling/tree-sitter-elm scanner approach.
 * Uses virtual tokens to handle layout-sensitive syntax:
 *   - VIRTUAL_END_DECL:    Separates items at same indent level (semicolon)
 *   - VIRTUAL_OPEN_SECTION: Opens a layout block (after 'of', 'do')
 *   - VIRTUAL_END_SECTION:  Closes a layout block when indent decreases
 *
 * Also handles identifier/constructor/type_variable disambiguation via column checks.
 */

#include "tree_sitter/parser.h"
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef DEBUG_SCANNER
#include <stdio.h>
#define DEBUG(...) fprintf(stderr, __VA_ARGS__)
#else
#define DEBUG(...)
#endif

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MAX_INDENT_DEPTH 256

// Vector macros for dynamic arrays
#define VEC_RESIZE(vec, _cap)                                          \
    void *tmp = realloc((vec).data, (_cap) * sizeof((vec).data[0]));   \
    assert(tmp != NULL);                                               \
    (vec).data = tmp;                                                  \
    (vec).cap = (_cap);

#define VEC_GROW(vec, _cap)                                            \
    if ((vec).cap < (_cap)) {                                          \
        VEC_RESIZE((vec), (_cap));                                     \
    }

#define VEC_PUSH(vec, el)                                              \
    if ((vec).cap == (vec).len) {                                      \
        VEC_RESIZE((vec), MAX(16, (vec).len * 2));                     \
    }                                                                  \
    (vec).data[(vec).len++] = (el);

#define VEC_POP(vec) (vec).len--;

#define VEC_BACK(vec) ((vec).data[(vec).len - 1])

#define VEC_FREE(vec)                                                  \
    {                                                                  \
        if ((vec).data != NULL)                                        \
            free((vec).data);                                          \
    }

#define VEC_CLEAR(vec) (vec).len = 0;

#define VEC_REVERSE(vec)                                               \
    do {                                                               \
        if ((vec).len > 1) {                                           \
            for (size_t i = 0, j = (vec).len - 1; i < j; i++, j--) {   \
                uint8_t tmp = (vec).data[i];                           \
                (vec).data[i] = (vec).data[j];                         \
                (vec).data[j] = tmp;                                   \
            }                                                          \
        }                                                              \
    } while (0)

// Token types - order must match externals in grammar.js
enum TokenType {
    VIRTUAL_END_DECL,      // Separates items at same indent level
    VIRTUAL_OPEN_SECTION,  // Opens a layout block
    VIRTUAL_END_SECTION,   // Closes a layout block
    EXPR_IDENTIFIER,       // Identifier in expression context
    EXPR_CONSTRUCTOR,      // Constructor in expression context
    TYPE_VARIABLE,         // Type variable (lowercase in type context)
};

// Runback token types
#define RUNBACK_END_DECL    0
#define RUNBACK_END_SECTION 1

typedef struct {
    uint32_t len;
    uint32_t cap;
    uint8_t *data;
} vec;

typedef struct {
    uint32_t indent_length;  // Current line indentation
    vec indents;             // Stack of indentation levels
    vec runback;             // Queued tokens for later emission
} Scanner;

// ============================================================================
// Helper functions
// ============================================================================

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }
static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

static bool in_error_recovery(const bool *valid_symbols) {
    return (valid_symbols[VIRTUAL_END_DECL] &&
            valid_symbols[VIRTUAL_OPEN_SECTION] &&
            valid_symbols[VIRTUAL_END_SECTION] &&
            valid_symbols[EXPR_IDENTIFIER] &&
            valid_symbols[EXPR_CONSTRUCTOR] &&
            valid_symbols[TYPE_VARIABLE]);
}

static bool is_whitespace(int32_t c) {
    return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

static bool is_identifier_start(int32_t c) {
    return (c >= 'a' && c <= 'z') || c == '_';
}

static bool is_constructor_start(int32_t c) {
    return (c >= 'A' && c <= 'Z');
}

static bool is_identifier_continue(int32_t c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
           (c >= '0' && c <= '9') || c == '_';
}

static bool is_keyword(const char *str, unsigned len) {
    const char *keywords[] = {
        "module", "exposing", "import", "as", "type", "alias", "newtype",
        "class", "instance", "where", "let", "in", "case", "of", "if",
        "then", "else", "do", "foreign", "ccall"
    };
    const int num_keywords = sizeof(keywords) / sizeof(keywords[0]);

    for (int i = 0; i < num_keywords; i++) {
        const char *kw = keywords[i];
        unsigned kw_len = strlen(kw);
        if (len == kw_len && memcmp(str, kw, len) == 0) return true;
    }
    return false;
}

// Check if we're at 'in' keyword (for let expressions)
static int check_for_in(TSLexer *lexer, const bool *valid_symbols) {
    if (valid_symbols[VIRTUAL_END_SECTION] && lexer->lookahead == 'i') {
        skip(lexer);
        if (lexer->lookahead == 'n') {
            skip(lexer);
            if (is_whitespace(lexer->lookahead) || lexer->eof(lexer)) {
                return 2; // Found 'in'
            }
            return 1; // Partial match
        }
        return 1; // Partial match
    }
    return 0;
}

// Check if we're at a token that should close a section
static bool check_section_ending_token(TSLexer *lexer, const bool *valid_symbols) {
    if (valid_symbols[VIRTUAL_END_SECTION] &&
        (lexer->lookahead == ')' || lexer->lookahead == ',' ||
         lexer->lookahead == '}' || lexer->lookahead == ']')) {
        return true;
    }
    return false;
}

static void advance_to_line_end(TSLexer *lexer) {
    while (!lexer->eof(lexer) && lexer->lookahead != '\n') {
        advance(lexer);
    }
}

// ============================================================================
// Main scanning logic
// ============================================================================

static bool scan(Scanner *scanner, TSLexer *lexer, const bool *valid_symbols) {
    if (in_error_recovery(valid_symbols)) {
        return false;
    }

    // First handle queued runback tokens from previous scan
    if (scanner->runback.len > 0 && VEC_BACK(scanner->runback) == RUNBACK_END_DECL &&
        valid_symbols[VIRTUAL_END_DECL]) {
        VEC_POP(scanner->runback);
        lexer->result_symbol = VIRTUAL_END_DECL;
        DEBUG("RUNBACK -> VIRTUAL_END_DECL\n");
        return true;
    }
    if (scanner->runback.len > 0 && VEC_BACK(scanner->runback) == RUNBACK_END_SECTION &&
        valid_symbols[VIRTUAL_END_SECTION]) {
        VEC_POP(scanner->runback);
        lexer->result_symbol = VIRTUAL_END_SECTION;
        DEBUG("RUNBACK -> VIRTUAL_END_SECTION\n");
        return true;
    }
    VEC_CLEAR(scanner->runback);

    // Track newlines and measure indentation
    bool has_newline = false;
    bool found_in = false;
    bool can_call_mark_end = true;
    lexer->mark_end(lexer);

    while (true) {
        if (lexer->lookahead == ' ' || lexer->lookahead == '\t' || lexer->lookahead == '\r') {
            skip(lexer);
        } else if (lexer->lookahead == '\n') {
            skip(lexer);
            has_newline = true;
            // Measure indentation after newline
            while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
                skip(lexer);
            }
            scanner->indent_length = lexer->get_column(lexer);
        } else if (lexer->lookahead == '-') {
            // Peek ahead to check for line comment (--)
            // We only consume if it IS a comment; otherwise break out and let
            // the main loop continue, leaving the '-' for the grammar to handle
            lexer->mark_end(lexer);  // Mark position before consuming
            advance(lexer);
            if (lexer->lookahead == '-' && has_newline) {
                // It's a line comment, consume it
                can_call_mark_end = false;
                advance(lexer);
                advance_to_line_end(lexer);
                // Continue the whitespace loop to check for more whitespace/comments
            } else {
                // Not a comment - this is a '-' operator or part of '<-'
                // Break out and let the rest of the scanner handle it
                // (mark_end was called before consuming, so we're safe)
                break;
            }
        } else if (lexer->eof(lexer)) {
            if (valid_symbols[VIRTUAL_END_SECTION]) {
                lexer->result_symbol = VIRTUAL_END_SECTION;
                return true;
            }
            if (valid_symbols[VIRTUAL_END_DECL]) {
                lexer->result_symbol = VIRTUAL_END_DECL;
                return true;
            }
            break;
        } else {
            break;
        }
    }

    uint32_t column = lexer->get_column(lexer);

    DEBUG("SCAN col=%d char='%c' newline=%d valid: VED=%d VOS=%d VES=%d EI=%d EC=%d TV=%d\n",
        column, lexer->lookahead > 31 ? lexer->lookahead : '?', has_newline,
        valid_symbols[VIRTUAL_END_DECL], valid_symbols[VIRTUAL_OPEN_SECTION],
        valid_symbols[VIRTUAL_END_SECTION], valid_symbols[EXPR_IDENTIFIER],
        valid_symbols[EXPR_CONSTRUCTOR], valid_symbols[TYPE_VARIABLE]);

    // Check for 'in' keyword closing a let expression
    if (check_for_in(lexer, valid_symbols) == 2) {
        if (has_newline) {
            found_in = true;
        } else {
            lexer->result_symbol = VIRTUAL_END_SECTION;
            if (scanner->indents.len > 0) {
                VEC_POP(scanner->indents);
            }
            DEBUG("  -> VIRTUAL_END_SECTION (in keyword)\n");
            return true;
        }
    }

    // Check for section-ending tokens: ) , } ]
    if (check_section_ending_token(lexer, valid_symbols)) {
        lexer->result_symbol = VIRTUAL_END_SECTION;
        if (scanner->indents.len > 0) {
            VEC_POP(scanner->indents);
        }
        DEBUG("  -> VIRTUAL_END_SECTION (closing token)\n");
        return true;
    }

    // Open a new layout section
    if (valid_symbols[VIRTUAL_OPEN_SECTION] && !lexer->eof(lexer)) {
        if (scanner->indents.len >= MAX_INDENT_DEPTH) {
            return false;
        }
        VEC_PUSH(scanner->indents, column);
        lexer->result_symbol = VIRTUAL_OPEN_SECTION;
        DEBUG("  -> VIRTUAL_OPEN_SECTION indent=%d (depth=%d)\n", column, scanner->indents.len);
        return true;
    }

    // Handle newline-based layout decisions ONLY if layout tokens are valid
    // This prevents premature END_DECL emission when scanning identifiers
    if (has_newline && (valid_symbols[VIRTUAL_END_DECL] || valid_symbols[VIRTUAL_END_SECTION])) {
        VEC_CLEAR(scanner->runback);

        // Compare indent to stack and queue tokens to emit
        while (scanner->indents.len > 0 && scanner->indent_length <= VEC_BACK(scanner->indents)) {
            if (scanner->indent_length == VEC_BACK(scanner->indents)) {
                if (found_in) {
                    VEC_POP(scanner->indents);
                    VEC_PUSH(scanner->runback, RUNBACK_END_SECTION);
                    found_in = false;
                    break;
                }
                // Same indent = new item in same block (semicolon)
                VEC_PUSH(scanner->runback, RUNBACK_END_DECL);
                break;
            }
            if (scanner->indent_length < VEC_BACK(scanner->indents)) {
                // Less indent = close this block
                VEC_POP(scanner->indents);
                VEC_PUSH(scanner->runback, RUNBACK_END_SECTION);
                if (found_in && (scanner->indents.len == 0 ||
                    scanner->indent_length > VEC_BACK(scanner->indents))) {
                    found_in = false;
                }
            }
        }

        // Handle remaining 'in' keyword
        if (found_in && scanner->indents.len > 0) {
            VEC_POP(scanner->indents);
            VEC_PUSH(scanner->runback, RUNBACK_END_SECTION);
        }

        // Reverse the runback queue (we built it backwards)
        VEC_REVERSE(scanner->runback);

        // Emit the first queued token
        if (scanner->runback.len > 0 && VEC_BACK(scanner->runback) == RUNBACK_END_DECL &&
            valid_symbols[VIRTUAL_END_DECL]) {
            VEC_POP(scanner->runback);
            lexer->result_symbol = VIRTUAL_END_DECL;
            DEBUG("  -> VIRTUAL_END_DECL (newline)\n");
            return true;
        }
        if (scanner->runback.len > 0 && VEC_BACK(scanner->runback) == RUNBACK_END_SECTION &&
            valid_symbols[VIRTUAL_END_SECTION]) {
            VEC_POP(scanner->runback);
            lexer->result_symbol = VIRTUAL_END_SECTION;
            DEBUG("  -> VIRTUAL_END_SECTION (newline)\n");
            return true;
        }
        if (lexer->eof(lexer) && valid_symbols[VIRTUAL_END_SECTION]) {
            lexer->result_symbol = VIRTUAL_END_SECTION;
            return true;
        }
    }

    // Handle constructor identifiers
    if (is_constructor_start(lexer->lookahead) && valid_symbols[EXPR_CONSTRUCTOR]) {
        while (is_identifier_continue(lexer->lookahead)) {
            advance(lexer);
        }
        lexer->mark_end(lexer);
        lexer->result_symbol = EXPR_CONSTRUCTOR;
        DEBUG("  -> EXPR_CONSTRUCTOR\n");
        return true;
    }

    // Handle lowercase identifiers (TYPE_VARIABLE and EXPR_IDENTIFIER)
    if (is_identifier_start(lexer->lookahead) &&
        (valid_symbols[TYPE_VARIABLE] || valid_symbols[EXPR_IDENTIFIER])) {

        uint32_t start_column = column;

        // Consume the identifier
        char buffer[64];
        unsigned len = 0;
        while (is_identifier_continue(lexer->lookahead)) {
            if (len < sizeof(buffer) - 1) {
                buffer[len++] = (char)lexer->lookahead;
            }
            advance(lexer);
        }
        buffer[len] = '\0';

        // Don't match keywords
        if (is_keyword(buffer, len)) {
            return false;
        }

        // Don't match lone underscore (wildcard pattern)
        if (len == 1 && buffer[0] == '_') {
            return false;
        }

        lexer->mark_end(lexer);

        // At column 0, check if this looks like a function declaration
        if (start_column == 0) {
            // Skip whitespace and check for = or :
            while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
                skip(lexer);
            }

            if (lexer->lookahead == '=' || lexer->lookahead == ':') {
                return false; // This is a function declaration
            }

            // Check for pattern: identifier+ =
            while (is_identifier_start(lexer->lookahead)) {
                while (is_identifier_continue(lexer->lookahead)) {
                    skip(lexer);
                }
                while (lexer->lookahead == ' ' || lexer->lookahead == '\t') {
                    skip(lexer);
                }
                if (lexer->lookahead == '=') {
                    return false; // This is a function declaration
                }
                if (lexer->lookahead == '\n' || lexer->lookahead == '\r' || lexer->eof(lexer)) {
                    break;
                }
            }
        }

        lexer->result_symbol = valid_symbols[TYPE_VARIABLE] ? TYPE_VARIABLE : EXPR_IDENTIFIER;
        DEBUG("  -> %s '%s' col=%d\n",
              lexer->result_symbol == TYPE_VARIABLE ? "TYPE_VARIABLE" : "EXPR_IDENTIFIER",
              buffer, start_column);
        return true;
    }

    return false;
}

// ============================================================================
// API functions
// ============================================================================

void *tree_sitter_lune_external_scanner_create(void) {
    Scanner *scanner = (Scanner *)calloc(1, sizeof(Scanner));
    VEC_PUSH(scanner->indents, 0);  // Start with top-level indent
    return scanner;
}

void tree_sitter_lune_external_scanner_destroy(void *payload) {
    Scanner *scanner = (Scanner *)payload;
    VEC_FREE(scanner->indents);
    VEC_FREE(scanner->runback);
    free(scanner);
}

unsigned tree_sitter_lune_external_scanner_serialize(void *payload, char *buffer) {
    Scanner *scanner = (Scanner *)payload;
    size_t size = 0;

    if (3 + scanner->indents.len + scanner->runback.len >= TREE_SITTER_SERIALIZATION_BUFFER_SIZE) {
        return 0;
    }

    // Serialize runback queue
    size_t runback_count = scanner->runback.len;
    if (runback_count > UINT8_MAX) runback_count = UINT8_MAX;
    buffer[size++] = (char)runback_count;
    if (runback_count > 0) {
        memcpy(&buffer[size], scanner->runback.data, runback_count);
    }
    size += runback_count;

    // Serialize indent_length
    size_t indent_length_length = sizeof(scanner->indent_length);
    buffer[size++] = (char)indent_length_length;
    if (indent_length_length > 0) {
        memcpy(&buffer[size], &scanner->indent_length, indent_length_length);
    }
    size += indent_length_length;

    // Serialize indents stack (skip first element which is always 0)
    for (uint32_t i = 1; i < scanner->indents.len && size < TREE_SITTER_SERIALIZATION_BUFFER_SIZE; i++) {
        buffer[size++] = (char)scanner->indents.data[i];
    }

    return size;
}

void tree_sitter_lune_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
    Scanner *scanner = (Scanner *)payload;
    VEC_CLEAR(scanner->runback);
    VEC_CLEAR(scanner->indents);
    VEC_PUSH(scanner->indents, 0);

    if (length == 0) return;

    size_t size = 0;

    // Deserialize runback queue
    size_t runback_count = (unsigned char)buffer[size++];
    VEC_GROW(scanner->runback, runback_count);
    if (runback_count > 0) {
        memcpy(scanner->runback.data, &buffer[size], runback_count);
        scanner->runback.len = runback_count;
        size += runback_count;
    }

    // Deserialize indent_length
    size_t indent_length_length = (unsigned char)buffer[size++];
    if (indent_length_length > 0) {
        memcpy(&scanner->indent_length, &buffer[size], indent_length_length);
        size += indent_length_length;
    }

    // Deserialize indents stack
    for (; size < length; size++) {
        VEC_PUSH(scanner->indents, (unsigned char)buffer[size]);
    }
}

bool tree_sitter_lune_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    Scanner *scanner = (Scanner *)payload;
    return scan(scanner, lexer, valid_symbols);
}
