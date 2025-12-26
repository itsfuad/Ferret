package ast

import "compiler/internal/source"

// CommentGroup represents a doc comment block attached to a declaration.
type CommentGroup struct {
	Text     string
	Location source.Location
}
