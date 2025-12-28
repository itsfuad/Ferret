package main

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, "bootstrap:", err)
		os.Exit(1)
	}

	fmt.Println("Done")
}

func run() error {
	root, err := findRepoRoot()
	if err != nil {
		return err
	}

	libsDir := filepath.Join(root, "libs")
	if err := os.MkdirAll(libsDir, 0755); err != nil {
		return fmt.Errorf("create libs dir: %w", err)
	}

	if err := syncFerretLibs(filepath.Join(root, "ferret_libs"), libsDir); err != nil {
		return err
	}

	if err := buildRuntimeLib(filepath.Join(root, "runtime"), libsDir); err != nil {
		return err
	}

	if err := copyToolchain(libsDir); err != nil {
		return err
	}

	return nil
}

func findRepoRoot() (string, error) {
	cwd, err := os.Getwd()
	if err != nil {
		return "", fmt.Errorf("get cwd: %w", err)
	}

	dir := cwd
	for {
		if fileExists(filepath.Join(dir, "go.mod")) {
			return dir, nil
		}
		next := filepath.Dir(dir)
		if next == dir {
			break
		}
		dir = next
	}

	return "", fmt.Errorf("go.mod not found from %s", cwd)
}

func syncFerretLibs(srcDir, destDir string) error {
	info, err := os.Stat(srcDir)
	if err != nil {
		return fmt.Errorf("ferret_libs not found: %w", err)
	}
	if !info.IsDir() {
		return fmt.Errorf("ferret_libs is not a directory: %s", srcDir)
	}

	return filepath.WalkDir(srcDir, func(path string, entry os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(srcDir, path)
		if err != nil {
			return err
		}
		if rel == "." {
			return nil
		}

		destPath := filepath.Join(destDir, rel)
		if entry.IsDir() {
			return os.MkdirAll(destPath, 0755)
		}
		if filepath.Ext(path) != ".fer" {
			return nil
		}
		return copyFile(path, destPath)
	})
}

func buildRuntimeLib(runtimeDir, libsDir string) error {
	cFiles, err := filepath.Glob(filepath.Join(runtimeDir, "*.c"))
	if err != nil {
		return fmt.Errorf("scan runtime sources: %w", err)
	}
	if len(cFiles) == 0 {
		return fmt.Errorf("no runtime C files found in %s", runtimeDir)
	}

	cc, err := resolveTool("FERRET_CC", "CC", defaultCompiler())
	if err != nil {
		return err
	}
	ar, err := resolveTool("AR", "", "ar")
	if err != nil {
		return err
	}

	objDir, err := os.MkdirTemp("", "ferret-rt-*")
	if err != nil {
		return fmt.Errorf("create temp dir: %w", err)
	}
	defer os.RemoveAll(objDir)

	for _, src := range cFiles {
		obj := filepath.Join(objDir, strings.TrimSuffix(filepath.Base(src), ".c")+".o")
		args := []string{"-std=c99", "-O2", "-w"}
		if runtime.GOOS == "linux" {
			args = append(args, "-fno-pie")
		}
		args = append(args, "-I", runtimeDir, "-c", src, "-o", obj)

		cmd := exec.Command(cc, args...)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return fmt.Errorf("compile %s: %w", filepath.Base(src), err)
		}
	}

	objFiles, err := filepath.Glob(filepath.Join(objDir, "*.o"))
	if err != nil {
		return fmt.Errorf("list objects: %w", err)
	}
	if len(objFiles) == 0 {
		return fmt.Errorf("no runtime objects built")
	}

	libPath := filepath.Join(libsDir, "libferret_runtime.a")
	arArgs := append([]string{"rcs", libPath}, objFiles...)
	arCmd := exec.Command(ar, arArgs...)
	arCmd.Stdout = os.Stdout
	arCmd.Stderr = os.Stderr
	if err := arCmd.Run(); err != nil {
		return fmt.Errorf("archive runtime library: %w", err)
	}

	if ranlib, err := exec.LookPath("ranlib"); err == nil {
		ranCmd := exec.Command(ranlib, libPath)
		ranCmd.Stdout = os.Stdout
		ranCmd.Stderr = os.Stderr
		if err := ranCmd.Run(); err != nil {
			return fmt.Errorf("ranlib failed: %w", err)
		}
	}

	return nil
}

func copyToolchain(libsDir string) error {
	toolchainDir := filepath.Join(libsDir, "toolchain")
	if err := os.MkdirAll(toolchainDir, 0755); err != nil {
		return fmt.Errorf("create toolchain dir: %w", err)
	}
	libDir := filepath.Join(toolchainDir, "lib")
	if err := os.MkdirAll(libDir, 0755); err != nil {
		return fmt.Errorf("create toolchain lib dir: %w", err)
	}

	asPath, err := resolveTool("FERRET_AS", "AS", "as")
	if err != nil {
		return err
	}
	ldPath, err := resolveTool("FERRET_LD", "LD", "ld")
	if err != nil {
		return err
	}

	if err := copyFile(asPath, filepath.Join(toolchainDir, "as")); err != nil {
		return fmt.Errorf("copy as: %w", err)
	}
	if err := copyFile(ldPath, filepath.Join(toolchainDir, "ld")); err != nil {
		return fmt.Errorf("copy ld: %w", err)
	}

	if err := copyToolchainDeps(toolchainDir, libDir, asPath, ldPath); err != nil {
		return err
	}

	return nil
}

func resolveTool(primaryEnv, fallbackEnv, defaultName string) (string, error) {
	if primaryEnv != "" {
		if val := os.Getenv(primaryEnv); val != "" {
			return resolveToolPath(val)
		}
	}
	if fallbackEnv != "" {
		if val := os.Getenv(fallbackEnv); val != "" {
			return resolveToolPath(val)
		}
	}
	return resolveToolPath(defaultName)
}

func resolveToolPath(name string) (string, error) {
	path, err := exec.LookPath(name)
	if err != nil {
		return "", fmt.Errorf("tool not found: %s", name)
	}
	return path, nil
}

func defaultCompiler() string {
	if runtime.GOOS == "darwin" {
		if _, err := exec.LookPath("clang"); err == nil {
			return "clang"
		}
	}
	return "gcc"
}

func copyFile(src, dst string) error {
	if err := os.MkdirAll(filepath.Dir(dst), 0755); err != nil {
		return err
	}

	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()

	info, err := in.Stat()
	if err != nil {
		return err
	}

	out, err := os.OpenFile(dst, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, info.Mode())
	if err != nil {
		return err
	}

	if _, err := io.Copy(out, in); err != nil {
		return err
	}
	return out.Close()
}

func fileExists(path string) bool {
	info, err := os.Stat(path)
	return err == nil && !info.IsDir()
}

func copyToolchainDeps(toolchainDir, libDir, asPath, ldPath string) error {
	switch runtime.GOOS {
	case "linux":
		return copyLinuxToolchain(libDir, asPath, ldPath)
	default:
		return copyBinaryDeps(libDir, asPath, ldPath)
	}
}

func copyLinuxToolchain(libDir, asPath, ldPath string) error {
	cc, err := resolveTool("FERRET_CC", "CC", defaultCompiler())
	if err != nil {
		return err
	}

	crt1 := gccPrintFile(cc, "crt1.o")
	crti := gccPrintFile(cc, "crti.o")
	crtn := gccPrintFile(cc, "crtn.o")
	libc := gccPrintFile(cc, "libc.so")
	libm := gccPrintFile(cc, "libm.so")
	libgcc := gccPrintFile(cc, "libgcc_s.so.1")
	if libgcc == "" {
		libgcc = gccPrintFile(cc, "libgcc_s.so")
	}

	if err := copyIfExists(libDir, crt1, crti, crtn, libc, libm, libgcc); err != nil {
		return err
	}

	if loader := findLinuxDynamicLoader(cc); loader != "" {
		if err := copyIfExists(libDir, loader); err != nil {
			return err
		}
	}

	if err := copyBinaryDeps(libDir, asPath, ldPath); err != nil {
		return err
	}

	return nil
}

func copyBinaryDeps(libDir string, binaries ...string) error {
	switch runtime.GOOS {
	case "linux":
		return copyWithLdd(libDir, binaries...)
	case "darwin":
		return copyWithOtool(libDir, binaries...)
	default:
		return nil
	}
}

func copyWithLdd(libDir string, binaries ...string) error {
	seen := map[string]struct{}{}
	for _, bin := range binaries {
		if bin == "" {
			continue
		}
		paths, err := lddPaths(bin)
		if err != nil {
			return err
		}
		for _, path := range paths {
			if _, ok := seen[path]; ok {
				continue
			}
			seen[path] = struct{}{}
			if err := copyIfExists(libDir, path); err != nil {
				return err
			}
		}
	}
	return nil
}

func copyWithOtool(libDir string, binaries ...string) error {
	seen := map[string]struct{}{}
	for _, bin := range binaries {
		if bin == "" {
			continue
		}
		paths, err := otoolPaths(bin)
		if err != nil {
			return err
		}
		for _, path := range paths {
			if _, ok := seen[path]; ok {
				continue
			}
			seen[path] = struct{}{}
			if err := copyIfExists(libDir, path); err != nil {
				return err
			}
		}
	}
	return nil
}

func lddPaths(bin string) ([]string, error) {
	out, err := exec.Command("ldd", bin).CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("ldd %s: %w", filepath.Base(bin), err)
	}
	lines := strings.Split(string(out), "\n")
	paths := make([]string, 0, len(lines))
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "linux-vdso") {
			continue
		}
		if strings.Contains(line, "=>") {
			parts := strings.Split(line, "=>")
			if len(parts) < 2 {
				continue
			}
			right := strings.TrimSpace(parts[1])
			path := strings.Fields(right)
			if len(path) == 0 || path[0] == "not" {
				continue
			}
			paths = append(paths, path[0])
			continue
		}
		fields := strings.Fields(line)
		if len(fields) > 0 && strings.HasPrefix(fields[0], "/") {
			paths = append(paths, fields[0])
		}
	}
	return paths, nil
}

func otoolPaths(bin string) ([]string, error) {
	out, err := exec.Command("otool", "-L", bin).CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("otool %s: %w", filepath.Base(bin), err)
	}
	lines := strings.Split(string(out), "\n")
	paths := make([]string, 0, len(lines))
	for i, line := range lines {
		if i == 0 {
			continue
		}
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		fields := strings.Fields(line)
		if len(fields) == 0 {
			continue
		}
		path := fields[0]
		if strings.HasPrefix(path, "/") {
			paths = append(paths, path)
		}
	}
	return paths, nil
}

func copyIfExists(destDir string, paths ...string) error {
	for _, path := range paths {
		if path == "" || !fileExists(path) {
			continue
		}
		if err := copyFile(path, filepath.Join(destDir, filepath.Base(path))); err != nil {
			return err
		}
	}
	return nil
}

func gccPrintFile(cc, name string) string {
	out, err := exec.Command(cc, "-print-file-name="+name).CombinedOutput()
	if err != nil {
		return ""
	}
	path := strings.TrimSpace(string(out))
	if path == "" || path == name {
		return ""
	}
	if !fileExists(path) {
		return ""
	}
	return path
}

func findLinuxDynamicLoader(cc string) string {
	candidates := linuxLoaderCandidates()
	for _, name := range candidates {
		if path := gccPrintFile(cc, name); path != "" {
			return path
		}
	}

	dirs := gccLibDirs(cc)
	for _, dir := range dirs {
		for _, name := range candidates {
			path := filepath.Join(dir, name)
			if fileExists(path) {
				return path
			}
		}
	}

	return ""
}

func linuxLoaderCandidates() []string {
	switch runtime.GOARCH {
	case "amd64":
		return []string{"ld-linux-x86-64.so.2", "ld-musl-x86_64.so.1"}
	case "arm64":
		return []string{"ld-linux-aarch64.so.1", "ld-musl-aarch64.so.1"}
	case "386":
		return []string{"ld-linux.so.2", "ld-musl-i386.so.1"}
	default:
		return []string{"ld-linux.so.2", "ld-musl.so.1"}
	}
}

func gccLibDirs(cc string) []string {
	out, err := exec.Command(cc, "-print-search-dirs").CombinedOutput()
	if err != nil {
		return nil
	}
	lines := strings.Split(string(out), "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if !strings.HasPrefix(line, "libraries:") {
			continue
		}
		parts := strings.SplitN(line, "=", 2)
		if len(parts) != 2 {
			continue
		}
		raw := strings.TrimSpace(parts[1])
		if raw == "" {
			continue
		}
		chunks := strings.Split(raw, string(os.PathListSeparator))
		dirs := make([]string, 0, len(chunks))
		for _, chunk := range chunks {
			if chunk == "" {
				continue
			}
			dirs = append(dirs, chunk)
		}
		return dirs
	}
	return nil
}
