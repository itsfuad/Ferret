package ferretlibs

import "embed"

//go:embed *.fer std/*.fer
var FS embed.FS
