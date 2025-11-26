# Build Scripts

## Available Scripts

### `build-all.bat` / `build-all.sh`
Builds both native executable and WebAssembly module in one command.

**Windows:**
```batch
scripts\build-all.bat
# or from root:
build-all.bat
```

**Linux/Mac:**
```bash
scripts/build-all.sh
# or from root:
./build-all.sh
```

**Output:**
- `bin/ferret.exe` (Windows) or `bin/ferret` (Linux/Mac) - Native executable
- `bin/ferret.wasm` - WebAssembly module for web integration

---

### `build.bat`
Builds only the native executable (Windows).

```batch
scripts\build.bat
```

**Output:** `bin/ferret.exe`

---

### `run.bat`
Runs the compiler using `go run` (development mode).

```batch
scripts\run.bat <file-path>
```

**Example:**
```batch
scripts\run.bat tests/showcase.fer
```

---

### `ex.bat`
Shorthand for running examples (development mode).

```batch
scripts\ex.bat <file-path>
```

## Quick Start

1. **Build everything:**
   ```batch
   build-all.bat
   ```

2. **Run the compiler:**
   ```batch
   bin\ferret.exe example.fer
   ```

3. **Or use development mode:**
   ```batch
   scripts\run.bat example.fer
   ```
