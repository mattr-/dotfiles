# AGENTS.md

This file provides guidance to agentic coding assistants when working in this dotfiles repository.

## Repository Overview

This is a cross-platform dotfiles repository managed by **chezmoi**. The main areas include:
- Neovim configuration (`home/dot_config/nvim/`)
- ZSH configuration (`home/dot_zsh/`)
- Custom utility scripts (`home/bin/`)
- Chezmoi install/configuration scripts (`home/.chezmoiscripts/`)
- Nix modules (`modules/`)

## Repository Transition Status

**IMPORTANT**: This repository is currently in transition from a chezmoi-based configuration to a Nix/NixOS/home-manager-based configuration.

### Transition Guidelines

- **Do NOT modify or remove existing chezmoi implementations** - they remain the primary configuration system
- **Tool overlap is acceptable and preferred** - the same tool may be managed by multiple systems:
  - chezmoi + mise (legacy)
  - Nix/home-manager (future)
  - Mason/Neovim (editor-specific)
  - Homebrew (macOS-specific)
- **Gradual migration** - new functionality should be added to Nix modules when possible, but existing chezmoi configs should remain untouched
- **Coexistence is intentional** - both systems will run in parallel during the transition period

### Which System to Use When

- **For new tool additions**: Add to Nix modules (`modules/base/*.nix`) when possible
- **For existing chezmoi configs**: Leave them alone unless explicitly instructed to migrate
- **For tool version management**: Accept that multiple systems may manage the same tool
- **For shell/environment setup**: chezmoi manages dotfiles, Nix manages binaries

The transition is ongoing and intentional overlap ensures nothing breaks during migration.

## Tool Management

This repository uses multiple tool managers due to the ongoing transition:

### Active Tool Managers (Listed by Priority)

1. **chezmoi**: Primary dotfile management system (current)
   - Source directory: `home/`
   - Manages: Shell configs, editor configs, scripts
   - Command: `chezmoi apply` (always use `chezmoi diff` first)

2. **Nix/home-manager**: Future system-level tool management (in transition)
   - Module directory: `modules/`
   - Manages: System packages, development tools
   - **CRITICAL WARNING**: See "Nix/Home-Manager Safety Warnings" below

3. **mise** (formerly asdf): Tool version management
   - Config: `home/dot_tool-versions`
   - Manages: Runtime versions (Ruby, Node, Go, etc.)
   - Command: `mise install`

4. **Mason** (Neovim): Editor-specific tools
   - Config: `home/dot_config/nvim/lua/dashvim/plugins/lsp/init.lua`
   - Manages: LSP servers, formatters, linters for Neovim
   - Command: `:Mason` in Neovim

5. **Homebrew** (macOS only): System packages
   - Config: `home/.chezmoiscripts/run_once_before_install-brew-bundle.sh.tmpl`
   - Manages: macOS applications and packages
   - Automatically run by chezmoi

### Tool Overlap Examples

The following tools are intentionally managed by multiple systems:
- `shellcheck`: mise (0.8.0) + Nix (unstable) + Mason (Neovim)
- `stylua`: Mason (Neovim) + potentially Nix in future
- `shfmt`: Mason (Neovim) + potentially Nix in future

This overlap is **acceptable and preferred** during the transition.

### Nix/Home-Manager Safety Warnings

**CRITICAL**: The following commands will COMPLETELY REPLACE your current home-manager configuration:

```bash
# DANGEROUS - Will clobber existing config!
nix run home-manager -- switch --flake .#mattr-
nix run home-manager -- switch --flake .#devcontainer --impure
darwin-rebuild switch --flake .
nixos-rebuild switch --flake .
```

**DO NOT USE** these commands without explicit user permission. They will replace the current system configuration.

**Safe approach for testing Nix modules**:
1. Review module changes: `git diff modules/`
2. Validate flake syntax: `nix flake check`
3. Inspect packages (read-only): `nix eval .#homeConfigurations.mattr-.config.home.packages --apply builtins.length`
4. **Never apply changes** without explicit user approval

**Safe approach for chezmoi**:
```bash
# Always preview before applying
chezmoi diff

# Only then apply if changes look correct
chezmoi apply
```

## Build/Lint/Test Commands

### Chezmoi Commands
```bash
# Apply dotfile changes after editing source files
chezmoi apply

# Preview what chezmoi will change (use before apply)
chezmoi diff

# Edit a managed file (opens source, applies on save)
chezmoi edit ~/.zshrc

# Initial bootstrap (installs chezmoi + applies config)
script/bootstrap
```

### Neovim Testing
```bash
# Test LazyVim in a Docker container
script/nvim/lazy_test

# Clean and restart test container
script/nvim/lazy_test -c

# Test DashVim configuration
script/nvim/dash_test
```

### Linting/Formatting
```bash
# Format Lua files (Neovim config)
stylua home/dot_config/nvim/

# Format shell scripts
shfmt -w -i 2 script/
```

## Chezmoi File Naming Conventions

Understanding chezmoi's file naming is critical when editing this repository:
- `home/` is the source directory (set by `.chezmoiroot`)
- `dot_` prefix becomes `.` (e.g., `dot_zshrc` → `~/.zshrc`)
- `executable_` prefix makes files executable (e.g., `executable_git-wtf` → executable `git-wtf`)
- `.tmpl` suffix indicates Go templates processed by chezmoi
- `symlink_` prefix creates symlinks
- Platform variables are in `home/.chezmoi.toml.tmpl`

**IMPORTANT**: When editing dotfiles, always modify files in `home/`, never directly in `~/`.

## Code Style Guidelines

### General Comment Policy

**CRITICAL**: Unless explicitly requested by the user:
- Do NOT add inline comments to code
- Do NOT add explanatory comments
- Do NOT add descriptive comments
- Code should be self-documenting through clear naming

When comments ARE requested:
- Use ONLY standard ASCII characters (no Unicode, emoji, or special symbols)
- Avoid characters like: em dash (—), en dash (–), fancy quotes (" "), bullet points (•), etc.
- Use standard ASCII alternatives: hyphen (-), straight quotes (" '), asterisk (*), etc.
- Keep comments concise and focused on "why" not "what"

Exception: None. Module-level documentation comments should NOT be present in this codebase.

### Indentation and Formatting

**CRITICAL**: Preserve existing indentation and formatting exactly:
- Do NOT reformat code unless explicitly requested
- When using the Edit tool, match the exact indentation of surrounding code
- Use `git diff` to verify no unintended whitespace changes
- If you accidentally change indentation, fix it immediately
- Respect the existing style even if it differs from language conventions

### Lua (Neovim Configuration)

**Formatting**:
- Use `stylua` with 2-space indentation and 120 character line width
- Config: `home/dot_config/nvim/stylua.toml`
- Requires are sorted automatically by stylua

**Structure**:
- Plugin configs return a table of plugin specifications
- Use lazy.nvim plugin format with `opts`, `config`, `keys`, `dependencies`
- Organize plugins by category: `lsp.lua`, `editor.lua`, `coding.lua`, `ui.lua`, etc.

**Type Annotations**:
```lua
--- @class DashVim
--- @field config dashvim.config
local M = {}
```

**Naming Conventions**:
- Module tables: uppercase `M` or descriptive names
- Private functions: prefix with `_` or keep local
- Global namespace: `DashVim` for custom utilities

**Imports**:
```lua
local LazyUtil = require("lazy.core.util")  -- External dependencies first
local util = require("dashvim.util")        -- Then internal modules
```

**Error Handling**:
```lua
local have_mason, mlsp = pcall(require, "mason-lspconfig")
if have_mason then
  -- use mlsp safely
end
```

**Comments**:
- **Do not add comments unless explicitly requested by the user**
- Use only ASCII characters in comments (no Unicode, emoji, or special symbols)
- Use `--` for single-line comments
- Use `---` for documentation comments (LuaLS format)
- Use `-- TODO` for action items
- Explain "why" not "what" in comments when they are needed

### Shell Scripts

**Formatting**:
- 2-space indentation
- Use `set -e` at the top of scripts for early exit on errors
- Use `set -u` when appropriate (fail on undefined variables)

**Functions**:
```bash
is_darwin() { [ "$(uname -s)" = "Darwin" ]; }
is_linux() { [ "$(uname -s)" = "Linux" ]; }
```

**Error Handling**:
```bash
if ! command -v nix &>/dev/null; then
  echo "Error: nix not found" >&2
  exit 1
fi
```

**Quoting**:
- Always quote variables: `"$variable"`
- Use `$()` for command substitution, not backticks
- Heredocs for multi-line strings

**Comments**:
- **Do not add comments unless explicitly requested by the user**
- Use only ASCII characters in comments (no Unicode, emoji, or special symbols)
- Use `#` for comments
- Document complex logic only when necessary

### Ruby Scripts

**Style** (from `git-merge-pull`):
- Use single quotes for strings unless interpolation needed
- Standard Ruby 2-space indentation
- Use `abort` for error exit messages
- Require statements at the top

**Error Handling**:
```ruby
begin
  # operation
rescue Octokit::NotFound => e
  abort "Error message"
end
```

**Comments**:
- **Do not add comments unless explicitly requested by the user**
- Use only ASCII characters in comments (no Unicode, emoji, or special symbols)
- Use `#` for comments
- Use YARD format for documentation when needed

## Architecture Patterns

### Neovim Plugin Organization

Plugin files should return a table of plugin specs:
```lua
return {
  {
    "plugin/name",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = { "other/plugin" },
    opts = {
      -- plugin options
    },
    config = function(_, opts)
      -- setup code
    end,
  },
}
```

### DashVim Global Namespace

Custom utilities are accessed via `DashVim`:
```lua
DashVim.config.icons.diagnostics
DashVim.util.lsp.on_attach()
DashVim.lazy.opts("plugin.name")
```

### ZSH Modular Loading

ZSH uses the `sourcedir` function to load files from subdirectories:
- `home/dot_zsh/zshrc/` - Runtime configuration
- `home/dot_zsh/zshenv/` - Environment variables
- `home/dot_zsh/functions/` - Shell functions

## Common Patterns

### Adding New Neovim Plugins

1. Add to appropriate category file in `lua/dashvim/plugins/`
2. Follow lazy.nvim spec format
3. Use `event`, `cmd`, or `keys` for lazy loading
4. Document keybindings in `desc` field

### Adding Custom Scripts

1. Create in `home/bin/`
2. Prefix with `executable_` for chezmoi
3. Use appropriate shebang (`#!/bin/bash`, `#!/usr/bin/env ruby`, etc.)
4. Include usage/help comments at the top

### Platform-Specific Configuration

Use Go templates in `.tmpl` files:
```go
{{- if eq .chezmoi.os "darwin" -}}
# macOS specific
{{- else if eq .chezmoi.os "linux" -}}
# Linux specific
{{- end -}}
```

## Testing Changes

1. **Before applying**: Always run `chezmoi diff` to preview changes
2. **Safe apply**: Test on non-critical dotfiles first
3. **Neovim**: Use test scripts to validate config in isolation
4. **Shell**: Source ZSH files in a subshell to test syntax

## Important Notes

- Never commit sensitive data (`.env`, credentials) - they're gitignored
- Chezmoi scripts in `home/.chezmoiscripts/` run automatically:
  - `run_once_before_*` - Package installation
  - `run_once_after_*` - Tool configuration
- The `.chezmoiroot` file sets `home/` as the source directory
- Git helpers are custom scripts, not standard git commands
- **Repository is in transition from chezmoi to Nix** - do not remove existing chezmoi configs
- **NEVER run `nix run home-manager -- switch` or similar commands without explicit user permission** - they will replace the current configuration
- When working with Nix modules, use `nix flake check` for validation instead of applying changes
- **Do not add code comments unless explicitly requested by the user**
- **All comments must use standard ASCII characters only** - no Unicode, emoji, or special symbols
