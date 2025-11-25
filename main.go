package main

import (
	"fmt"
	"os"
	"path/filepath"

	"compiler/colors"
	"compiler/internal/context_v2"
	"compiler/internal/pipeline"
)

const VERSION = "0.1.0"

func main() {
	if len(os.Args) < 2 {
		printUsage()
		os.Exit(1)
	}

	// Parse command line arguments
	var (
		debug     bool
		inputFile string
		showHelp  bool
	)

	for i := 1; i < len(os.Args); i++ {
		arg := os.Args[i]
		switch arg {
		case "-debug", "--debug", "-d":
			debug = true
		case "-version", "--version", "-v":
			fmt.Printf("Ferret compiler version %s\n", VERSION)
			os.Exit(0)
		case "-help", "--help", "-h":
			showHelp = true
		case "get":
			handleGet(os.Args[i+1:])
			return
		case "update":
			handleUpdate(os.Args[i+1:])
			return
		case "delete":
			handleDelete(os.Args[i+1:])
			return
		case "sniff":
			handleSniff(os.Args[i+1:])
			return
		default:
			if arg[0] != '-' && inputFile == "" {
				inputFile = arg
			}
		}
	}

	if showHelp {
		printUsage()
		os.Exit(0)
	}

	if inputFile == "" {
		colors.RED.Println("Error: no input file specified")
		printUsage()
		os.Exit(1)
	}

	// Compile the file
	ctx := compile(inputFile, debug)

	// Emit diagnostics if any
	ctx.EmitDiagnostics()

	// Exit with error if compilation failed
	if ctx.HasErrors() {
		os.Exit(1)
	}

	// Success
	if debug {
		colors.GREEN.Println("\n✓ Compilation successful!")
	}
}

// compile does parse, analyze, and compile the source code
func compile(inputFile string, debug bool) *context_v2.CompilerContext {
	// Resolve absolute path
	absPath, err := filepath.Abs(inputFile)
	if err != nil {
		colors.RED.Printf("❌ Failed to resolve path: %v\n", err)
		os.Exit(1)
	}

	// Check file exists
	if _, err := os.Stat(absPath); os.IsNotExist(err) {
		colors.RED.Printf("❌ Entry point file not found: %s\n", inputFile)
		os.Exit(1)
	}

	// Get project root (directory containing the file)
	projectRoot := filepath.Dir(absPath)
	projectName := filepath.Base(projectRoot)

	// Find builtin modules (standard library)
	execPath, err := os.Executable()
	if err != nil {
		colors.RED.Printf("❌ Failed to get executable path: %v\n", err)
		os.Exit(1)
	}
	builtinPath := filepath.Join(filepath.Dir(execPath), "../modules")

	if debug {
		colors.BLUE.Printf("🚀 Running project with entry point: %s\n", inputFile)
	}

	// Create compiler config
	config := &context_v2.Config{
		ProjectName:        projectName,
		ProjectRoot:        projectRoot,
		Extension:          ".fer",
		BuiltinModulesPath: builtinPath,
		OutputPath:         filepath.Join(projectRoot, "build", projectName),
	}

	// Create compiler context
	ctx := context_v2.New(config, debug)

	// Set entry point
	if err := ctx.SetEntryPoint(absPath); err != nil {
		ctx.ReportError(fmt.Sprintf("Failed to set entry point: %v", err), nil)
		return ctx
	}

	// Create and run pipeline
	p := pipeline.New(ctx, debug)
	if err := p.Run(); err != nil {
		// Errors are already reported in context
		return ctx
	}

	// Check for critical errors
	if ctx.HasErrors() {
		return ctx
	}

	if debug {
		colors.BLUE.Printf("---------- [Parsing done] ----------\n")
	}

	// Print summary if debug
	if debug {
		p.PrintSummary()
	}

	return ctx
}

func printUsage() {
	fmt.Println(`Ferret - A modern systems programming language

Usage:
  ferret [options] <file>              Compile a Ferret source file
  ferret get <repo>                    Install a remote package
  ferret update <repo>                 Update an installed package
  ferret delete <repo>                 Remove an installed package
  ferret sniff <repo>                  Check for package updates

Options:
  -d, --debug, -debug                  Enable debug output
  -v, --version, -version              Show version information
  -h, --help, -help                    Show this help message

Examples:
  ferret main.fer                      Compile main.fer
  ferret -d main.fer                   Compile with debug output
  ferret get github.com/user/package   Install a package
  ferret update github.com/user/pkg    Update a package
  ferret delete github.com/user/pkg    Remove a package
  ferret sniff github.com/user/pkg     Check for updates

For more information, visit: https://github.com/yourusername/ferret`)
}

// Remote package management commands (future implementation)

func handleGet(args []string) {
	if len(args) == 0 {
		colors.RED.Println("Error: repository name required")
		fmt.Println("Usage: ferret get <repository>")
		os.Exit(1)
	}

	repo := args[0]
	colors.YELLOW.Printf("Installing %s...\n", repo)
	colors.YELLOW.Println("Remote package management not yet implemented")
	colors.YELLOW.Println("This feature will be available in a future release")

	// TODO: Implement remote package installation
	// 1. Parse repository URL (github.com/user/repo)
	// 2. Clone or download the repository
	// 3. Extract version information
	// 4. Copy to cache directory (.ferret/)
	// 5. Update project dependencies
}

func handleUpdate(args []string) {
	if len(args) == 0 {
		colors.RED.Println("Error: repository name required")
		fmt.Println("Usage: ferret update <repository>")
		os.Exit(1)
	}

	repo := args[0]
	colors.YELLOW.Printf("Updating %s...\n", repo)
	colors.YELLOW.Println("Remote package management not yet implemented")
	colors.YELLOW.Println("This feature will be available in a future release")

	// TODO: Implement package update
	// 1. Check current version
	// 2. Fetch latest version
	// 3. Download and install if newer version exists
}

func handleDelete(args []string) {
	if len(args) == 0 {
		colors.RED.Println("Error: repository name required")
		fmt.Println("Usage: ferret delete <repository>")
		os.Exit(1)
	}

	repo := args[0]
	colors.YELLOW.Printf("Removing %s...\n", repo)
	colors.YELLOW.Println("Remote package management not yet implemented")
	colors.YELLOW.Println("This feature will be available in a future release")

	// TODO: Implement package removal
	// 1. Check if package is installed
	// 2. Remove from cache directory
	// 3. Update project dependencies
}

func handleSniff(args []string) {
	if len(args) == 0 {
		colors.RED.Println("Error: repository name required")
		fmt.Println("Usage: ferret sniff <repository>")
		os.Exit(1)
	}

	repo := args[0]
	colors.YELLOW.Printf("Checking updates for %s...\n", repo)
	colors.YELLOW.Println("Remote package management not yet implemented")
	colors.YELLOW.Println("This feature will be available in a future release")

	// TODO: Implement update checking
	// 1. Get current installed version
	// 2. Fetch latest version from remote
	// 3. Compare and report if update is available
}
