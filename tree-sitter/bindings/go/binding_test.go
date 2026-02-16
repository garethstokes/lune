package tree_sitter_lune_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_lune "github.com/garethstokes/lune/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_lune.Language())
	if language == nil {
		t.Errorf("Error loading Lune grammar")
	}
}
