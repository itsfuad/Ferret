package phase

// ModulePhase tracks the compilation phase of an individual module
//
// Phase progression must be sequential and respect dependencies:
// - NotStarted -> Lexed -> Parsed (currently implemented)
// - Parsed -> Collected -> Resolved -> TypeChecked -> HIRGenerated -> CFGAnalyzed -> HIRLowered -> CodeGen
//
// Phase transitions are validated using AdvanceModulePhase() which checks
// that prerequisites are satisfied via the phasePrerequisites map.
//
// Phase prerequisites (enforced by tests in pipeline_test.go):
// - A module at PhaseResolved requires all its imports at least PhaseParsed
// - A module at PhaseTypeChecked requires all its imports at least PhaseResolved
type ModulePhase int

const (
	PhaseNotStarted   ModulePhase = iota // Module discovered but not processed
	PhaseLexed                           // Tokens generated
	PhaseParsed                          // AST built
	PhaseCollected                       // Symbols collected (first pass)
	PhaseResolved                        // Imports and symbols resolved
	PhaseTypeChecked                     // Type checking complete
	PhaseHIRGenerated                    // HIR generation complete
	PhaseCFGAnalyzed                     // Control flow analysis complete
	PhaseHIRLowered                      // HIR lowering complete
	PhaseCodeGen                         // Code generation complete
)

// PhasePrerequisites maps each phase to its required predecessor phase
// This explicit mapping is safer than arithmetic and allows for non-linear phase progressions
var PhasePrerequisites = map[ModulePhase]ModulePhase{
	PhaseLexed:        PhaseNotStarted,
	PhaseParsed:       PhaseLexed,
	PhaseCollected:    PhaseParsed,
	PhaseResolved:     PhaseCollected,
	PhaseTypeChecked:  PhaseResolved,
	PhaseHIRGenerated: PhaseTypeChecked,
	PhaseCFGAnalyzed:  PhaseHIRGenerated,
	PhaseHIRLowered:   PhaseCFGAnalyzed,
	PhaseCodeGen:      PhaseHIRLowered,
}

func (p ModulePhase) String() string {
	switch p {
	case PhaseNotStarted:
		return "NotStarted"
	case PhaseLexed:
		return "Lexed"
	case PhaseParsed:
		return "Parsed"
	case PhaseCollected:
		return "Collected"
	case PhaseResolved:
		return "Resolved"
	case PhaseTypeChecked:
		return "TypeChecked"
	case PhaseHIRGenerated:
		return "HIRGenerated"
	case PhaseCFGAnalyzed:
		return "CFGAnalyzed"
	case PhaseHIRLowered:
		return "HIRLowered"
	case PhaseCodeGen:
		return "CodeGen"
	default:
		return "Unknown"
	}
}
