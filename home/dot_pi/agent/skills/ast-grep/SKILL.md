---
name: ast-grep
description: Structural source code search using ast-grep. Use this skill instead of rg/grep when searching for code patterns like function calls, definitions, imports, class members, or any construct where whitespace/formatting variation would defeat text matching. Handles JavaScript, TypeScript, Python, Rust, Go, Java, C, C++, Ruby, and many more languages.
---

# ast-grep

`ast-grep` matches code by structure, not text. It ignores formatting differences and understands language syntax - a pattern like `foo($ARG)` matches `foo(x)`, `foo( x )`, and multiline calls equally.

## Core Command

```bash
ast-grep run -p '<pattern>' -l <lang> [paths...]
```

- Omit `-l`/`--lang` to let ast-grep auto-detect from file extensions (usually reliable)
- `paths` defaults to `.` (entire working directory)

## Metavariable Syntax

| Syntax      | Matches                          | Example                        |
|-------------|----------------------------------|--------------------------------|
| `$NAME`     | Any single AST node              | `foo($ARG)` - one argument     |
| `$$$NAME`   | Zero or more nodes (variadic)    | `foo($$$ARGS)` - any arg count |
| `$_`        | Any single node, unnamed         | `if ($_) { $$$BODY }`          |
| `$$$`       | Any sequence, unnamed            | `[$$$]`                        |

Metavariable names must be ALL_CAPS after the `$`.

## Common Patterns by Language

### JavaScript / TypeScript (`-l js`, `-l ts`, `-l jsx`, `-l tsx`)

```bash
# All calls to a specific function
ast-grep run -p 'console.log($$$ARGS)' -l js

# Function declarations
ast-grep run -p 'function $NAME($$$PARAMS) { $$$BODY }' -l js

# Arrow functions assigned to a variable
ast-grep run -p 'const $NAME = ($$$PARAMS) => $$$BODY' -l js

# import statements
ast-grep run -p 'import $NAME from "$MODULE"' -l ts
ast-grep run -p 'import { $$$NAMES } from "$MODULE"' -l ts

# await calls
ast-grep run -p 'await $EXPR' -l ts

# Object method calls
ast-grep run -p '$OBJ.$METHOD($$$ARGS)' -l js

# try/catch blocks
ast-grep run -p 'try { $$$TRY } catch ($E) { $$$CATCH }' -l js

# React hooks
ast-grep run -p 'useState($INIT)' -l tsx
ast-grep run -p 'useEffect(() => { $$$BODY }, [$$$DEPS])' -l tsx
```

### Python (`-l python`)

```bash
# Function definitions
ast-grep run -p 'def $NAME($$$PARAMS): $$$BODY' -l python

# Class definitions
ast-grep run -p 'class $NAME: $$$BODY' -l python
ast-grep run -p 'class $NAME($PARENT): $$$BODY' -l python

# Decorated functions
ast-grep run -p '@$DECORATOR
def $NAME($$$PARAMS): $$$BODY' -l python

# Import statements
ast-grep run -p 'import $MODULE' -l python
ast-grep run -p 'from $MODULE import $NAME' -l python

# With statements
ast-grep run -p 'with $EXPR as $VAR: $$$BODY' -l python

# Assert statements
ast-grep run -p 'assert $COND, $MSG' -l python
```

### Rust (`-l rust`)

```bash
# Function definitions
ast-grep run -p 'fn $NAME($$$PARAMS) -> $RET { $$$BODY }' -l rust
ast-grep run -p 'fn $NAME($$$PARAMS) { $$$BODY }' -l rust

# use declarations
ast-grep run -p 'use $$$PATH::$NAME;' -l rust

# struct definitions
ast-grep run -p 'struct $NAME { $$$FIELDS }' -l rust

# impl blocks
ast-grep run -p 'impl $TYPE { $$$METHODS }' -l rust

# match expressions
ast-grep run -p 'match $EXPR { $$$ARMS }' -l rust

# unwrap/expect chains
ast-grep run -p '$EXPR.unwrap()' -l rust
ast-grep run -p '$EXPR.expect($MSG)' -l rust

# Macro calls
ast-grep run -p 'println!($$$ARGS)' -l rust
```

### Go (`-l go`)

```bash
# Function declarations
ast-grep run -p 'func $NAME($$$PARAMS) $$$RESULTS { $$$BODY }' -l go

# Method declarations
ast-grep run -p 'func ($RECV $TYPE) $NAME($$$PARAMS) $$$RESULTS { $$$BODY }' -l go

# Error checks
ast-grep run -p 'if err != nil { $$$BODY }' -l go

# Short variable declarations
ast-grep run -p '$VAR := $EXPR' -l go

# Goroutines
ast-grep run -p 'go $FUNC($$$ARGS)' -l go
```

### Java (`-l java`)

```bash
# Method declarations
ast-grep run -p 'public $RET $NAME($$$PARAMS) { $$$BODY }' -l java

# Class declarations
ast-grep run -p 'class $NAME extends $PARENT { $$$BODY }' -l java

# Annotations
ast-grep run -p '@$ANNOTATION
$$$REST' -l java
```

## Useful Flags

```bash
# Show context around matches
ast-grep run -p '<pattern>' -l js -C 3        # 3 lines before and after
ast-grep run -p '<pattern>' -l js -A 2 -B 1   # 2 after, 1 before

# List only files with matches (like rg -l)
ast-grep run -p '<pattern>' -l python --files-with-matches

# Structured JSON output (useful for piping/processing)
ast-grep run -p '<pattern>' -l ts --json

# Search specific files or directories
ast-grep run -p '<pattern>' -l js src/components/

# Filter by glob
ast-grep run -p '<pattern>' -l ts --globs '**/*.test.ts'
ast-grep run -p '<pattern>' -l ts --globs '!**/node_modules/**'

# Match by AST node kind alone (no pattern text)
ast-grep run --kind function_declaration -l js
ast-grep run --kind import_statement -l ts

# Explore AST structure to build patterns (pipe code sample via stdin)
echo 'your_code_here' | ast-grep run -l js --pattern '$X' --stdin --debug-query=ast
```

## Exploring AST Structure

When you are unsure of a pattern, use `--debug-query` to inspect the tree:

```bash
# See the named AST nodes for a snippet
echo 'const x = foo?.bar()' | ast-grep run -l js -p '$X' --stdin --debug-query=ast

# See full CST including punctuation
echo 'const x = foo?.bar()' | ast-grep run -l js -p '$X' --stdin --debug-query=cst
```

The output labels each node with its kind. Use those kind names in `--kind` or to inform pattern structure.

## Strictness Levels

When the default match is too loose or too strict:

```bash
ast-grep run -p '<pattern>' -l js --strictness smart      # default: ignores whitespace/trivial nodes
ast-grep run -p '<pattern>' -l js --strictness ast        # match only named AST nodes
ast-grep run -p '<pattern>' -l js --strictness relaxed    # AST nodes, ignore comments
ast-grep run -p '<pattern>' -l js --strictness cst        # exact CST match (very strict)
```

## Multi-Language Projects

When a project mixes languages, run separate searches per language, or omit `-l` and let
ast-grep auto-detect:

```bash
# Auto-detect (searches all supported files in cwd)
ast-grep run -p 'TODO($$$)' .

# Or target specific subdirectories per language
ast-grep run -p '$$$' -l ts src/
ast-grep run -p '$$$' -l python scripts/
```

## When to Use ast-grep vs rg

- **Use `ast-grep`** when the pattern is a code construct: function calls, declarations, imports, class bodies, control flow. Especially when formatting may vary or the match spans lines.
- **Use `rg`** for plain-text search: comments, string literals, file names, config values, or when you already know the exact text.

Combining both is valid: use `ast-grep run --files-with-matches` to find relevant files, then `rg` inside them for exact strings.

## Supported Languages (partial list)

`js`, `ts`, `jsx`, `tsx`, `python`, `rust`, `go`, `java`, `c`, `cpp`, `cs` (C#), `ruby`, `php`, `swift`, `kotlin`, `scala`, `html`, `css`, `bash`, `json`, `yaml`, `toml`

Full list: https://ast-grep.github.io/reference/languages.html
