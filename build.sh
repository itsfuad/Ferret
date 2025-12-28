#!/bin/bash
set -e

go run ./tools
go build -v -o bin/ferret
