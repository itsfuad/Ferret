package codegen

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"compiler/colors"
	"compiler/internal/context_v2"
	utilsfs "compiler/internal/utils/fs"
)

// BuildOptions configures how to build the executable
type BuildOptions struct {
	Assembler   string   // Assembler (as)
	AsFlags     []string // Assembler flags
	Linker      string   // Linker (ld)
	LinkFlags   []string // Linker flags
	LinkInputs  []string // Linker inputs before objects (e.g., crt1/crti)
	LinkLibs    []string // Linker libraries after objects
	LinkPost    []string // Linker inputs after libraries (e.g., crtn)
	RuntimePath string   // Path to runtime library directory
	OutputPath  string   // Output executable path
	Debug       bool
	LdDefaults  bool // Whether ld defaults were applied
}

// DefaultBuildOptions returns default build options
func DefaultBuildOptions() *BuildOptions {
	linkFlags := []string{"-s"}
	assembler := os.Getenv("FERRET_AS")
	if assembler == "" {
		assembler = os.Getenv("AS")
	}

	linker := os.Getenv("FERRET_LD")
	if linker == "" {
		linker = os.Getenv("LD")
	}

	toolchainPath := resolveToolchainPath()
	if toolchainPath != "" {
		if assembler == "" {
			candidates := []string{filepath.Join(toolchainPath, "as")}
			if runtime.GOOS == "windows" {
				candidates = append([]string{filepath.Join(toolchainPath, "as.exe")}, candidates...)
			}
			for _, candidate := range candidates {
				if utilsfs.IsValidFile(candidate) {
					assembler = candidate
					break
				}
			}
		}
		if linker == "" {
			candidates := []string{filepath.Join(toolchainPath, "ld")}
			if runtime.GOOS == "windows" {
				candidates = append([]string{filepath.Join(toolchainPath, "ld.exe")}, candidates...)
			}
			for _, candidate := range candidates {
				if utilsfs.IsValidFile(candidate) {
					linker = candidate
					break
				}
			}
		}

		libDir := filepath.Join(toolchainPath, "lib")
		if utilsfs.IsDir(libDir) {
			linkFlags = append(linkFlags, "-L", libDir)
		}
	}

	if assembler == "" {
		assembler = "as"
	}
	if linker == "" {
		linker = "ld"
	}

	if !isLdLinker(linker) {
		switch runtime.GOOS {
		case "linux":
			linkFlags = append(linkFlags, "-no-pie")
		case "darwin":
			linkFlags = append(linkFlags, "-Wl,-no_pie")
		case "openbsd":
			linkFlags = append(linkFlags, "-nopie")
		case "freebsd":
			// FreeBSD defaults are acceptable; no PIE flag needed.
		}
	}

	linkLibs := []string{"-lm"}
	if runtime.GOOS == "windows" {
		linkLibs = []string{
			"-lmingw32",
			"-lmingwex",
			"-lmsvcrt",
			"-lkernel32",
			"-luser32",
			"-lgcc",
			"-lgcc_eh",
			"-lm",
		}
	}

	return &BuildOptions{
		Assembler:   assembler,
		Linker:      linker,
		LinkFlags:   linkFlags, // -s: strip symbols
		LinkLibs:    linkLibs,
		RuntimePath: "libs",
		Debug:       false,
	}
}

// BuildExecutable assembles QBE output and links with the runtime library.
func BuildExecutable(ctx *context_v2.CompilerContext, asmFiles []string, opts *BuildOptions) error {
	if opts == nil {
		opts = DefaultBuildOptions()
	}

	// Determine runtime path - use from config if available, otherwise from opts
	runtimePath := opts.RuntimePath
	if runtimePath == "" && ctx.Config.RuntimePath != "" {
		runtimePath = ctx.Config.RuntimePath
	}

	// Make it absolute
	if !filepath.IsAbs(runtimePath) {
		if absRuntime, err := filepath.Abs(runtimePath); err == nil {
			runtimePath = absRuntime
		}
	}

	// Check if runtime library directory exists
	if !utilsfs.IsDir(runtimePath) {
		return fmt.Errorf("runtime library directory not found: %s", runtimePath)
	}

	runtimeLib := filepath.Join(runtimePath, "libferret_runtime.a")
	if !utilsfs.IsValidFile(runtimeLib) {
		return fmt.Errorf("runtime library not found: %s", runtimeLib)
	}

	// Determine output path
	outputPath := opts.OutputPath
	if outputPath == "" {
		return fmt.Errorf("output path must be specified")
	}

	assembler := opts.Assembler
	if assembler == "" {
		assembler = "as"
	}

	objFiles := make([]string, 0, len(asmFiles))
	for _, asmFile := range asmFiles {
		if strings.TrimSpace(asmFile) == "" {
			continue
		}
		objPath := strings.TrimSuffix(asmFile, filepath.Ext(asmFile)) + ".o"
		args := append([]string{}, opts.AsFlags...)
		args = append(args, "-o", objPath, asmFile)

		if ctx.Config.Debug || opts.Debug {
			colors.CYAN.Printf("Assembling: %s %v\n", assembler, args)
		}

		cmd := exec.Command(assembler, args...)
		cmd.Env = toolchainEnv()
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return fmt.Errorf("assembly failed: %w", err)
		}
		objFiles = append(objFiles, objPath)
	}

	linker := opts.Linker
	if linker == "" {
		return fmt.Errorf("linker must be specified")
	}

	if err := applyLdDefaults(opts); err != nil {
		return err
	}

	args := []string{}
	args = append(args, opts.LinkFlags...)
	args = append(args, "-o", outputPath)
	args = append(args, opts.LinkInputs...)
	args = append(args, objFiles...)
	args = append(args, runtimeLib)
	args = append(args, opts.LinkLibs...)
	args = append(args, opts.LinkPost...)

	if ctx.Config.Debug || opts.Debug {
		colors.CYAN.Printf("Linking executable: %s %v\n", linker, args)
	}

	cmd := exec.Command(linker, args...)
	cmd.Env = toolchainEnv()
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		return fmt.Errorf("linking failed: %w", err)
	}

	if ctx.Config.Debug || opts.Debug {
		colors.GREEN.Printf("  ✓ Built: %s\n", outputPath)
	}

	return nil
}

func isLdLinker(linker string) bool {
	base := filepath.Base(linker)
	base = strings.TrimSuffix(strings.ToLower(base), ".exe")
	return base == "ld" || strings.HasPrefix(base, "ld.")
}

func applyLdDefaults(opts *BuildOptions) error {
	if opts == nil || opts.LdDefaults || !isLdLinker(opts.Linker) {
		return nil
	}
	opts.LdDefaults = true

	if extraFlags := strings.Fields(os.Getenv("FERRET_LD_FLAGS")); len(extraFlags) > 0 {
		opts.LinkFlags = append(opts.LinkFlags, extraFlags...)
	}
	if extraLibs := strings.Fields(os.Getenv("FERRET_LD_LIBS")); len(extraLibs) > 0 {
		opts.LinkLibs = append(opts.LinkLibs, extraLibs...)
	}

	switch runtime.GOOS {
	case "linux":
		crt1, crti, crtn := findLinuxCrtObjects()
		if crt1 == "" || crti == "" || crtn == "" {
			return fmt.Errorf("ld: missing C runtime objects; set FERRET_LD_CRT1, FERRET_LD_CRTI, and FERRET_LD_CRTN")
		}

		dyn := findLinuxDynamicLinker()
		if dyn == "" {
			return fmt.Errorf("ld: dynamic linker not found; set FERRET_LD_DYNAMIC_LINKER")
		}

		if !containsArg(opts.LinkFlags, "-dynamic-linker") {
			opts.LinkFlags = append(opts.LinkFlags, "-dynamic-linker", dyn)
		}

		opts.LinkInputs = append(opts.LinkInputs, crt1, crti)
		if !containsArg(opts.LinkLibs, "-lc") {
			opts.LinkLibs = append([]string{"-lc"}, opts.LinkLibs...)
		}
		opts.LinkPost = append(opts.LinkPost, crtn)
	case "android":
		return fmt.Errorf("android builds are not supported here; use install-termux.sh")
	case "windows":
		crt2, crtbegin, crtend := findWindowsCrtObjects()
		if crt2 == "" || crtbegin == "" || crtend == "" {
			return fmt.Errorf("ld: missing C runtime objects; set FERRET_LD_CRT2, FERRET_LD_CRTBEGIN, and FERRET_LD_CRTEND")
		}
		opts.LinkInputs = append(opts.LinkInputs, crt2, crtbegin)
		opts.LinkPost = append(opts.LinkPost, crtend)
	}

	return nil
}

func findLinuxCrtObjects() (string, string, string) {
	crt1 := os.Getenv("FERRET_LD_CRT1")
	crti := os.Getenv("FERRET_LD_CRTI")
	crtn := os.Getenv("FERRET_LD_CRTN")

	dirs := linuxLibDirs()
	if crt1 == "" {
		crt1 = firstExisting(dirs, "crt1.o")
	}
	if crti == "" {
		crti = firstExisting(dirs, "crti.o")
	}
	if crtn == "" {
		crtn = firstExisting(dirs, "crtn.o")
	}

	return crt1, crti, crtn
}

func findLinuxDynamicLinker() string {
	if override := os.Getenv("FERRET_LD_DYNAMIC_LINKER"); override != "" {
		return override
	}

	var candidates []string
	switch runtime.GOARCH {
	case "amd64":
		candidates = []string{"ld-linux-x86-64.so.2", "ld-musl-x86_64.so.1"}
	case "arm64":
		candidates = []string{"ld-linux-aarch64.so.1", "ld-musl-aarch64.so.1"}
	case "386":
		candidates = []string{"ld-linux.so.2", "ld-musl-i386.so.1"}
	default:
		candidates = []string{"ld-linux.so.2", "ld-musl.so.1"}
	}

	dirs := linuxLibDirs()
	for _, dir := range dirs {
		for _, name := range candidates {
			path := filepath.Join(dir, name)
			if utilsfs.IsValidFile(path) {
				return path
			}
		}
	}

	return ""
}

func linuxLibDirs() []string {
	dirs := []string{"/lib", "/usr/lib", "/lib64", "/usr/lib64"}
	if tc := toolchainLibDir(); tc != "" {
		dirs = append([]string{tc}, dirs...)
	}
	switch runtime.GOARCH {
	case "amd64":
		dirs = append([]string{"/lib/x86_64-linux-gnu", "/usr/lib/x86_64-linux-gnu"}, dirs...)
	case "arm64":
		dirs = append([]string{"/lib/aarch64-linux-gnu", "/usr/lib/aarch64-linux-gnu"}, dirs...)
	case "386":
		dirs = append([]string{"/lib/i386-linux-gnu", "/usr/lib/i386-linux-gnu"}, dirs...)
	}

	seen := make(map[string]struct{}, len(dirs))
	unique := make([]string, 0, len(dirs))
	for _, dir := range dirs {
		if _, ok := seen[dir]; ok {
			continue
		}
		seen[dir] = struct{}{}
		unique = append(unique, dir)
	}
	return unique
}

func windowsLibDirs() []string {
	dirs := []string{}
	if tc := toolchainLibDir(); tc != "" {
		dirs = append(dirs, tc)
	}
	seen := make(map[string]struct{}, len(dirs))
	unique := make([]string, 0, len(dirs))
	for _, dir := range dirs {
		if _, ok := seen[dir]; ok {
			continue
		}
		seen[dir] = struct{}{}
		unique = append(unique, dir)
	}
	return unique
}

func findWindowsCrtObjects() (string, string, string) {
	crt2 := os.Getenv("FERRET_LD_CRT2")
	crtbegin := os.Getenv("FERRET_LD_CRTBEGIN")
	crtend := os.Getenv("FERRET_LD_CRTEND")

	dirs := windowsLibDirs()
	if crt2 == "" {
		crt2 = firstExisting(dirs, "crt2.o")
	}
	if crtbegin == "" {
		crtbegin = firstExisting(dirs, "crtbegin.o")
	}
	if crtend == "" {
		crtend = firstExisting(dirs, "crtend.o")
	}

	return crt2, crtbegin, crtend
}

func firstExisting(dirs []string, name string) string {
	for _, dir := range dirs {
		path := filepath.Join(dir, name)
		if utilsfs.IsValidFile(path) {
			return path
		}
	}
	return ""
}

func containsArg(args []string, value string) bool {
	for _, arg := range args {
		if arg == value {
			return true
		}
	}
	return false
}

func resolveToolchainPath() string {
	if override := os.Getenv("FERRET_TOOLCHAIN_PATH"); override != "" {
		if utilsfs.IsDir(override) {
			return override
		}
	}
	if libsOverride := os.Getenv("FERRET_LIBS_PATH"); libsOverride != "" {
		candidate := filepath.Join(libsOverride, "toolchain")
		if utilsfs.IsDir(candidate) {
			return candidate
		}
	}

	if execPath, err := os.Executable(); err == nil {
		candidate := filepath.Join(filepath.Dir(execPath), "../libs/toolchain")
		if utilsfs.IsDir(candidate) {
			return candidate
		}
	}

	if cwd, err := os.Getwd(); err == nil {
		candidate := filepath.Join(cwd, "libs", "toolchain")
		if utilsfs.IsDir(candidate) {
			return candidate
		}
	}

	return ""
}

func toolchainLibDir() string {
	if tc := resolveToolchainPath(); tc != "" {
		libDir := filepath.Join(tc, "lib")
		if utilsfs.IsDir(libDir) {
			return libDir
		}
	}
	return ""
}

func toolchainEnv() []string {
	env := os.Environ()
	libDir := toolchainLibDir()
	if libDir == "" {
		return env
	}

	switch runtime.GOOS {
	case "linux":
		return withPathEnv(env, "LD_LIBRARY_PATH", libDir)
	case "darwin":
		return withPathEnv(env, "DYLD_LIBRARY_PATH", libDir)
	case "windows":
		return withPathEnv(env, "PATH", libDir)
	default:
		return env
	}
}

func withPathEnv(env []string, key, dir string) []string {
	if dir == "" {
		return env
	}
	for i, kv := range env {
		if !strings.HasPrefix(kv, key+"=") {
			continue
		}
		current := strings.TrimPrefix(kv, key+"=")
		if current == "" {
			env[i] = key + "=" + dir
		} else {
			env[i] = key + "=" + dir + string(os.PathListSeparator) + current
		}
		return env
	}
	return append(env, key+"="+dir)
}
