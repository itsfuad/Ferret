package controlflow

import (
	"compiler/internal/context_v2"
	"compiler/internal/diagnostics"
	"compiler/internal/frontend/ast"
	"compiler/internal/source"
	"fmt"
)

// ControlFlowGraph represents the control flow structure of a function
type ControlFlowGraph struct {
	Entry *BasicBlock // Entry block
	Exit  *BasicBlock // Virtual exit block
}

// BasicBlock represents a sequence of statements with single entry and exit
type BasicBlock struct {
	ID           int              // Unique identifier
	Nodes        []ast.Node       // Statements in this block
	Successors   []*BasicBlock    // Possible next blocks
	Predecessors []*BasicBlock    // Blocks that can reach this one
	Terminator   ControlFlowKind  // How this block ends
	Location     *source.Location // Location for diagnostics
	Reachable    bool             // Whether this block is reachable
	Returns      bool             // Whether this block always returns
	CanFallThru  bool             // Whether execution can fall through
	BranchKind   string           // "then", "else", "loop", etc.
	OriginNode   ast.Node         // The AST node that created this branch
}

// ControlFlowKind represents how a basic block terminates
type ControlFlowKind int

const (
	FlowFallthrough ControlFlowKind = iota // Normal flow to next block
	FlowReturn                             // Return statement
	FlowBreak                              // Break statement
	FlowContinue                           // Continue statement
	FlowConditional                        // If/when branch
	FlowLoop                               // Loop construct
	FlowUnreachable                        // After return/panic/etc
)

// CFGBuilder builds control flow graphs from AST
type CFGBuilder struct {
	ctx          *context_v2.CompilerContext
	mod          *context_v2.Module
	blockCounter int
	currentLoop  *loopContext // Stack of loop contexts for break/continue
}

// loopContext tracks the current loop for break/continue statements
type loopContext struct {
	breakTarget    *BasicBlock
	continueTarget *BasicBlock
	parent         *loopContext
}

// NewCFGBuilder creates a new control flow graph builder
func NewCFGBuilder(ctx *context_v2.CompilerContext, mod *context_v2.Module) *CFGBuilder {
	return &CFGBuilder{
		ctx:          ctx,
		mod:          mod,
		blockCounter: 0,
		currentLoop:  nil,
	}
}

// newBlock creates a new basic block
func (b *CFGBuilder) newBlock() *BasicBlock {
	b.blockCounter++
	return &BasicBlock{
		ID:           b.blockCounter,
		Nodes:        make([]ast.Node, 0),
		Successors:   make([]*BasicBlock, 0),
		Predecessors: make([]*BasicBlock, 0),
		Terminator:   FlowFallthrough,
		Reachable:    false,
		Returns:      false,
		CanFallThru:  true,
	}
}

// addEdge adds a control flow edge between blocks
func addEdge(from, to *BasicBlock) {
	if from != nil && to != nil {
		from.Successors = append(from.Successors, to)
		to.Predecessors = append(to.Predecessors, from)
	}
}

// BuildFunctionCFG builds a control flow graph for a function
func (b *CFGBuilder) BuildFunctionCFG(funcDecl *ast.FuncDecl) *ControlFlowGraph {
	cfg := &ControlFlowGraph{
		Entry: b.newBlock(),
		Exit:  b.newBlock(),
	}

	cfg.Entry.Reachable = true
	cfg.Entry.Location = funcDecl.Loc()

	if funcDecl.Body != nil {
		current := cfg.Entry
		current = b.buildBlock(funcDecl.Body, current, cfg.Exit)

		// If current block can fall through, connect to exit
		if current != nil && current.CanFallThru {
			addEdge(current, cfg.Exit)
		}
	} else {
		// Empty function body - direct edge to exit
		addEdge(cfg.Entry, cfg.Exit)
	}

	return cfg
}

// buildBlock processes a block of statements
func (b *CFGBuilder) buildBlock(block *ast.Block, current *BasicBlock, exitBlock *BasicBlock) *BasicBlock {
	if block == nil {
		return current
	}

	// Track unreachable code ranges
	var unreachableStart ast.Node
	var unreachableEnd ast.Node

	for _, node := range block.Nodes {
		if current == nil {
			// We hit unreachable code after return/break/continue
			if unreachableStart == nil {
				unreachableStart = node
			}
			unreachableEnd = node
			continue
		} else {
			// Report accumulated unreachable code before processing reachable node
			if unreachableStart != nil {
				b.reportUnreachableCodeRange(unreachableStart, unreachableEnd)
				unreachableStart = nil
				unreachableEnd = nil
			}
		}

		current = b.buildNode(node, current, exitBlock)
	}

	// Report any remaining unreachable code at end of block
	if unreachableStart != nil {
		b.reportUnreachableCodeRange(unreachableStart, unreachableEnd)
	}

	return current
}

// buildNode processes a single AST node and returns the next basic block
func (b *CFGBuilder) buildNode(node ast.Node, current *BasicBlock, exitBlock *BasicBlock) *BasicBlock {
	if node == nil || current == nil {
		return current
	}

	switch n := node.(type) {
	case *ast.ReturnStmt:
		return b.buildReturn(n, current, exitBlock)

	case *ast.BreakStmt:
		return b.buildBreak(n, current)

	case *ast.ContinueStmt:
		return b.buildContinue(n, current)

	case *ast.IfStmt:
		return b.buildIf(n, current, exitBlock)

	case *ast.ForStmt:
		return b.buildFor(n, current, exitBlock)

	case *ast.WhileStmt:
		return b.buildWhile(n, current, exitBlock)

	case *ast.Block:
		return b.buildBlock(n, current, exitBlock)

	case *ast.DeclStmt:
		// Declarations don't affect control flow
		current.Nodes = append(current.Nodes, node)
		return current

	default:
		// Regular statements don't affect control flow
		current.Nodes = append(current.Nodes, node)
		return current
	}
}

// buildReturn handles return statements
func (b *CFGBuilder) buildReturn(stmt *ast.ReturnStmt, current *BasicBlock, exitBlock *BasicBlock) *BasicBlock {
	current.Nodes = append(current.Nodes, stmt)
	current.Terminator = FlowReturn
	current.Returns = true
	current.CanFallThru = false
	addEdge(current, exitBlock)
	return nil // Code after return is unreachable
}

// buildBreak handles break statements
func (b *CFGBuilder) buildBreak(stmt *ast.BreakStmt, current *BasicBlock) *BasicBlock {
	if b.currentLoop == nil {
		b.ctx.Diagnostics.Add(
			diagnostics.NewError("break statement outside loop").
				WithPrimaryLabel(stmt.Loc(), "not inside a loop"),
		)
		return current
	}

	current.Nodes = append(current.Nodes, stmt)
	current.Terminator = FlowBreak
	current.CanFallThru = false
	addEdge(current, b.currentLoop.breakTarget)
	return nil // Code after break is unreachable
}

// buildContinue handles continue statements
func (b *CFGBuilder) buildContinue(stmt *ast.ContinueStmt, current *BasicBlock) *BasicBlock {
	if b.currentLoop == nil {
		b.ctx.Diagnostics.Add(
			diagnostics.NewError("continue statement outside loop").
				WithPrimaryLabel(stmt.Loc(), "not inside a loop"),
		)
		return current
	}

	current.Nodes = append(current.Nodes, stmt)
	current.Terminator = FlowContinue
	current.CanFallThru = false
	addEdge(current, b.currentLoop.continueTarget)
	return nil // Code after continue is unreachable
}

// buildIf handles if statements with optional else
func (b *CFGBuilder) buildIf(stmt *ast.IfStmt, current *BasicBlock, exitBlock *BasicBlock) *BasicBlock {
	// Add condition to current block
	current.Nodes = append(current.Nodes, stmt)
	current.Terminator = FlowConditional

	// Create blocks for if and else branches
	ifBlock := b.newBlock()
	ifBlock.Reachable = current.Reachable
	ifBlock.Location = stmt.Body.Loc()
	ifBlock.BranchKind = "if"
	ifBlock.OriginNode = stmt
	addEdge(current, ifBlock)

	// Process if branch
	afterIf := b.buildBlock(stmt.Body, ifBlock, exitBlock)

	// Process else branch (if exists)
	var afterElse *BasicBlock
	if stmt.Else != nil {
		elseBlock := b.newBlock()
		elseBlock.Reachable = current.Reachable
		elseBlock.Location = stmt.Else.Loc()
		elseBlock.BranchKind = "else"
		elseBlock.OriginNode = stmt
		addEdge(current, elseBlock)

		// Check if else is another if-stmt (else if) or a block
		if elseIf, ok := stmt.Else.(*ast.IfStmt); ok {
			afterElse = b.buildIf(elseIf, elseBlock, exitBlock)
		} else if elseBlockAST, ok := stmt.Else.(*ast.Block); ok {
			afterElse = b.buildBlock(elseBlockAST, elseBlock, exitBlock)
		} else {
			afterElse = b.buildNode(stmt.Else, elseBlock, exitBlock)
		}
	}

	// Merge point after if-else
	mergeBlock := b.newBlock()
	mergeBlock.Location = stmt.Loc()

	// Connect branches to merge
	if afterIf != nil && afterIf.CanFallThru {
		addEdge(afterIf, mergeBlock)
		mergeBlock.Reachable = true
	}

	if stmt.Else != nil {
		// Has else branch
		if afterElse != nil && afterElse.CanFallThru {
			addEdge(afterElse, mergeBlock)
			mergeBlock.Reachable = true
		}

		// If both branches return/break/continue, merge is unreachable
		bothReturn := (afterIf == nil || !afterIf.CanFallThru) &&
			(afterElse == nil || !afterElse.CanFallThru)
		if bothReturn {
			mergeBlock.Reachable = false
			return nil
		}
	} else {
		// No else branch - always has fallthrough path
		addEdge(current, mergeBlock)
		mergeBlock.Reachable = true
	}

	return mergeBlock
}

// buildFor handles for loops
func (b *CFGBuilder) buildFor(stmt *ast.ForStmt, current *BasicBlock, exitBlock *BasicBlock) *BasicBlock {
	// For loops iterate over a range - always finite, cannot be infinite
	// Create loop header
	headerBlock := b.newBlock()
	headerBlock.Reachable = current.Reachable
	headerBlock.Location = stmt.Loc()
	headerBlock.Terminator = FlowLoop
	addEdge(current, headerBlock)

	// Create loop body
	bodyBlock := b.newBlock()
	bodyBlock.Reachable = headerBlock.Reachable
	bodyBlock.Location = stmt.Body.Loc()
	addEdge(headerBlock, bodyBlock)

	// After block (loop exit when range is exhausted)
	afterBlock := b.newBlock()
	afterBlock.Location = stmt.Loc()
	addEdge(headerBlock, afterBlock)

	// Set up loop context for break/continue
	oldLoop := b.currentLoop
	b.currentLoop = &loopContext{
		breakTarget:    afterBlock,
		continueTarget: headerBlock, // Continue goes back to range check
		parent:         oldLoop,
	}

	// Build loop body
	lastBlock := b.buildBlock(stmt.Body, bodyBlock, exitBlock)

	// Connect last block back to header for next iteration
	if lastBlock != nil && lastBlock.CanFallThru {
		addEdge(lastBlock, headerBlock)
	}

	// Restore loop context
	b.currentLoop = oldLoop

	afterBlock.Reachable = true // Can always exit when range is exhausted
	return afterBlock
}

// buildWhile handles while loops
func (b *CFGBuilder) buildWhile(stmt *ast.WhileStmt, current *BasicBlock, exitBlock *BasicBlock) *BasicBlock {
	// Create loop header (condition check)
	headerBlock := b.newBlock()
	headerBlock.Reachable = current.Reachable
	headerBlock.Location = stmt.Loc()
	headerBlock.Terminator = FlowLoop
	addEdge(current, headerBlock)

	// Create loop body
	bodyBlock := b.newBlock()
	bodyBlock.Reachable = headerBlock.Reachable
	bodyBlock.Location = stmt.Body.Loc()
	addEdge(headerBlock, bodyBlock)

	// After block (loop exit)
	afterBlock := b.newBlock()
	afterBlock.Location = stmt.Loc()
	addEdge(headerBlock, afterBlock) // Condition false exits loop

	// Set up loop context for break/continue
	oldLoop := b.currentLoop
	b.currentLoop = &loopContext{
		breakTarget:    afterBlock,
		continueTarget: headerBlock, // Continue goes back to condition
		parent:         oldLoop,
	}

	// Process body
	afterBody := b.buildBlock(stmt.Body, bodyBlock, exitBlock)

	// Connect body back to header
	if afterBody != nil && afterBody.CanFallThru {
		addEdge(afterBody, headerBlock)
	}

	// Restore loop context
	b.currentLoop = oldLoop

	afterBlock.Reachable = true // Can always exit via condition
	return afterBlock
}

// reportUnreachableCodeRange reports a diagnostic for a range of unreachable code
func (b *CFGBuilder) reportUnreachableCodeRange(start, end ast.Node) {
	if start == nil {
		return
	}

	// Create a location that spans from start to end
	startLoc := start.Loc()
	endLoc := end.Loc()
	if endLoc == nil {
		endLoc = startLoc
	}

	// Merge locations
	rangeLocation := &source.Location{
		Filename: start.Loc().Filename,
		Start:    startLoc.Start,
		End:      endLoc.End,
	}

	b.ctx.Diagnostics.Add(
		diagnostics.NewWarning("unreachable code").
			WithPrimaryLabel(rangeLocation, "this code will never execute").
			WithHelp("remove this code or restructure control flow"),
	)
}

// AnalyzeReturns checks if a function returns in all paths
func AnalyzeReturns(
	ctx *context_v2.CompilerContext,
	mod *context_v2.Module,
	funcDecl *ast.FuncDecl,
	cfg *ControlFlowGraph,
) {
	// Check if function requires a return value
	requiresReturn := funcDecl.Type != nil &&
		funcDecl.Type.Result != nil &&
		!isVoidType(funcDecl.Type.Result)

	if !requiresReturn {
		return // void functions don't need return
	}

	// Check if all paths lead to return
	if !allPathsReturn(cfg) {
		returnType := "value"
		if funcDecl.Type.Result != nil {
			if ident, ok := funcDecl.Type.Result.(*ast.IdentifierExpr); ok {
				returnType = ident.Name
			}
		}

		diag := diagnostics.NewError(
			fmt.Sprintf("not all code paths in function '%s' return a value of type %s",
				getFuncName(funcDecl), returnType)).
			WithPrimaryLabel(funcDecl.Name.Loc(),
				"missing return on some paths")

		// Find which branches are missing returns
		missingBranches := findMissingReturnBranches(cfg)

		if len(missingBranches) > 0 {
			// Add secondary labels for each branch that's missing a return
			for _, block := range missingBranches {
				if block.Location != nil {
					msg := "missing return in this branch"

					// Determine the branch type based on BranchKind
					if block.BranchKind == "else" {
						msg = fmt.Sprintf("missing return in else at line %d", block.Location.Start.Line)
					} else if block.BranchKind == "if" {
						msg = fmt.Sprintf("missing return in if at line %d", block.Location.Start.Line)
					} else if block.BranchKind != "" {
						msg = fmt.Sprintf("missing return in %s at line %d", block.BranchKind, block.Location.Start.Line)
					}

					diag.WithSecondaryLabel(block.Location, msg)
				}
			}
		}

		diag.WithHelp("make sure every branch returns, or add a final return at the end of the function")
		ctx.Diagnostics.Add(diag)
	}
}

// allPathsReturn checks if all paths through the CFG lead to a return
func allPathsReturn(cfg *ControlFlowGraph) bool {
	// DFS to check if we can reach exit without returning
	visited := make(map[*BasicBlock]bool)
	return !canReachExitWithoutReturn(cfg.Entry, cfg.Exit, visited)
}

// canReachExitWithoutReturn checks if exit is reachable without a return
func canReachExitWithoutReturn(current, exit *BasicBlock, visited map[*BasicBlock]bool) bool {
	if current == nil || visited[current] {
		return false
	}

	visited[current] = true

	// If we reached exit and this path doesn't return, we found a problem
	if current == exit {
		return true
	}

	// If this block returns, this path is OK
	if current.Returns {
		return false
	}

	// Check all successors
	for _, succ := range current.Successors {
		if canReachExitWithoutReturn(succ, exit, visited) {
			return true
		}
	}

	return false
}

// findMissingReturnBranches identifies blocks that can reach exit without returning
func findMissingReturnBranches(cfg *ControlFlowGraph) []*BasicBlock {
	// First, find all blocks that can reach exit without returning
	blocksReachingExit := make(map[*BasicBlock]bool)
	visited := make(map[*BasicBlock]bool)

	var findReachingExit func(*BasicBlock)
	findReachingExit = func(block *BasicBlock) {
		if block == nil || visited[block] || block == cfg.Exit {
			return
		}

		visited[block] = true

		// Check if this block reaches exit without returning
		for _, succ := range block.Successors {
			if succ == cfg.Exit && !block.Returns {
				blocksReachingExit[block] = true
			}
		}

		// Recurse
		for _, succ := range block.Successors {
			findReachingExit(succ)
		}
	}

	findReachingExit(cfg.Entry)

	// Now trace backwards from blocks reaching exit to find ALL their branch origins
	missing := make([]*BasicBlock, 0)
	seen := make(map[*BasicBlock]bool)

	for block := range blocksReachingExit {
		// If this block has a branch kind, it's a branch origin
		if block.BranchKind != "" {
			if !seen[block] {
				missing = append(missing, block)
				seen[block] = true
			}
			continue
		}

		// Otherwise, trace backwards to find ALL branches that lead here
		branches := traceToAllBranches(block)
		for _, branchBlock := range branches {
			if !seen[branchBlock] {
				missing = append(missing, branchBlock)
				seen[branchBlock] = true
			}
		}
	}

	return missing
}

// traceToAllBranches traces backwards from a block to find ALL branch origins
func traceToAllBranches(block *BasicBlock) []*BasicBlock {
	branches := make([]*BasicBlock, 0)
	visited := make(map[*BasicBlock]bool)
	queue := []*BasicBlock{block}

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if visited[current] {
			continue
		}
		visited[current] = true

		// If we found a branch, collect it and don't traverse further back
		if current.BranchKind != "" {
			branches = append(branches, current)
			continue // Don't traverse past branch origins
		}

		// Otherwise, check all predecessors
		queue = append(queue, current.Predecessors...)
	}

	return branches
}

// isVoidType checks if a type node represents void
func isVoidType(typeNode ast.TypeNode) bool {
	if typeNode == nil {
		return true
	}

	if ident, ok := typeNode.(*ast.IdentifierExpr); ok {
		return ident.Name == "void"
	}

	return false
}

// getFuncName returns the function name or "<anonymous>" for lambdas
func getFuncName(funcDecl *ast.FuncDecl) string {
	if funcDecl.Name != nil {
		return funcDecl.Name.Name
	}
	return "<anonymous>"
}
