---
title: Installation
description: How to install and set up Ferret
---

## Building from Source

Currently, Ferret is best installed by building from source:

```bash
# Clone the repository
git clone https://github.com/itsfuad/FCCIMPL.git
cd FCCIMPL

# Switch to the type checking branch
git checkout tc

# Build the compiler
go build -o bin/ferret.exe .
```

:::note
Before installing Ferret, make sure you have:
- Go 1.21 or later (for building from source)
:::

## Verify Installation

Check that Ferret is installed correctly:

```bash
# Windows
.\bin\ferret.exe --version

# Linux/macOS (coming soon)
./bin/ferret --version
```

## Running Ferret Programs

To compile and run a Ferret program:

```bash
.\bin\ferret.exe yourfile.fer
```