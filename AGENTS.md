# AGENTS.md

This file provides guidance to agentic coding assistants when working in this dotfiles repository.

## Repository Overview

This is a cross-platform dotfiles repository managed by **chezmoi**. The main areas include:
- Neovim configuration (`home/dot_config/nvim/`)
- ZSH configuration (`home/dot_zsh/`)
- Custom utility scripts (`home/bin/`)
- Chezmoi install/configuration scripts (`home/.chezmoiscripts/`)
- Nix modules (`modules/`) -- uses the [dendritic pattern](https://github.com/mightyiam/dendritic); see "Nix Module Architecture" below

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

- **For new tool additions**: Add to Nix modules (`modules/`) when possible; see "Nix Module Architecture" for how to create new modules
- **For existing chezmoi configs**: Leave them alone unless explicitly instructed to migrate
- **For tool version management**: Accept that multiple systems may manage the same tool
- **For shell/environment setup**: chezmoi manages dotfiles, Nix manages binaries

The transition is ongoing and intentional overlap ensures nothing breaks during migration.

## Nix Module Architecture (Dendritic Pattern)

The Nix configuration in `modules/` follows the [dendritic pattern](https://github.com/mightyiam/dendritic), where **every `.nix` file is a flake-parts module** of the same top-level configuration. Files are named by the feature they implement, not the platform they target. A single file can contribute configuration to multiple platforms (NixOS, nix-darwin, home-manager) simultaneously.

### Flake Entry Point

The `flake.nix` is intentionally minimal. The key mechanism is `import-tree`, which recursively discovers and imports every `.nix` file under `./modules/` as a flake-parts module. There is no explicit import list -- dropping a new `.nix` file into `modules/` (or any subdirectory) automatically includes it.

```nix
outputs = inputs:
  inputs.flake-parts.lib.mkFlake { inherit inputs; }
    (inputs.import-tree ./modules);
```

### Three-Tier Module System

Modules contribute configuration to one or more **tiers** by setting attributes under `flake.modules.<tier>.<name>`:

| Tier | Namespace | Consumed by | Purpose |
|---|---|---|---|
| NixOS | `flake.modules.nixos.<name>` | NixOS host definitions | System-level Linux configuration |
| Darwin | `flake.modules.darwin.<name>` | nix-darwin host definitions | System-level macOS configuration |
| Home Manager | `flake.modules.homeManager.<name>` | All host types + standalone profiles | User-level configuration |

Host definitions collect **all** modules from their relevant tiers using `builtins.attrValues`. This means every module in a tier applies to every host that consumes that tier -- there is no per-host module selection within a tier.

### Module Examples

**Single-tier module** (`modules/boot.nix`) -- contributes only to NixOS:

```nix
{ lib, ... }:
{
  flake.modules.nixos.boot = {
    boot.loader = {
      systemd-boot = {
        enable = lib.mkDefault true;
        consoleMode = lib.mkDefault "max";
        configurationLimit = 5;
      };
      efi.canTouchEfiVariables = lib.mkDefault true;
    };
  };
}
```

**Multi-tier module** (`modules/gnome.nix`) -- contributes to both NixOS and home-manager from a single file:

```nix
{ ... }:
{
  flake.modules.nixos.gnome = { pkgs, ... }: {
    services.desktopManager.gnome.enable = true;
    services.displayManager.gdm.enable = true;
    environment.systemPackages = with pkgs; [
      gnomeExtensions.appindicator
      gnomeExtensions.paperwm
    ];
  };

  flake.modules.homeManager.gnome = {
    dconf.settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
      };
    };
  };
}
```

**Host definition** (`modules/hosts/knid.nix`) -- collects all modules from relevant tiers:

```nix
{ inputs, config, ... }:
let
  nixosModules = builtins.attrValues (config.flake.modules.nixos or { });
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
in
{
  flake.nixosConfigurations."knid" = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; };
    modules = nixosModules ++ [
      inputs.home-manager.nixosModules.home-manager
      # ...host-specific modules and overrides...
      ({ ... }: {
        home-manager.users."mattr-" = { ... }: {
          imports = hmModules;
        };
      })
    ];
  };
}
```

### File Conventions

- **Underscore prefix** (`_hardware-configuration.nix`, `_disko.nix`): Files prefixed with `_` are excluded from `import-tree` auto-import. They are manually imported by their parent host file. Used for machine-specific hardware configs that should not be treated as standalone flake-parts modules.
- **Subdirectory grouping**: Subdirectories organize related modules but have no semantic meaning to the system. Current groups:
  - `base/` -- foundational cross-platform modules (git, nix, shell)
  - `editors/` -- editor-specific modules (neovim)
  - `hosts/` -- host/profile definitions with per-host subdirectories for hardware configs
- **Feature naming**: Files are named after the feature/concern they configure (e.g., `bluetooth.nix`, `wayland.nix`), not the platform they target.
- **Shared config via `let` bindings**: When multiple tiers need the same configuration, extract it into a `let` binding at the top of the file (see `modules/base/nix.nix` for an example with `sharedNixConfig`).
- **`mkDefault` for overridable defaults**: Use `lib.mkDefault` for settings that hosts may need to override (boot loader, locale, state version, etc.).

### How to Add a New Module

1. Create a new `.nix` file in `modules/` (or an appropriate subdirectory like `base/`, `editors/`)
2. Name the file after the feature it configures
3. Write a flake-parts module that sets one or more `flake.modules.<tier>.<name>` attributes
4. The file is automatically discovered by `import-tree` -- no registration needed
5. The module automatically applies to all hosts that consume its tier(s)
6. **Update the Module Inventory table below** with the new file

Example skeleton for a new module:

```nix
{ ... }:
{
  flake.modules.homeManager.myfeature = { pkgs, ... }: {
    home.packages = with pkgs; [ sometool ];
  };
}
```

### How to Add a New Host

1. Create a new `.nix` file in `modules/hosts/`
2. Collect the relevant tier modules using `builtins.attrValues (config.flake.modules.<tier> or { })`
3. Define the configuration using the appropriate builder (`lib.nixosSystem`, `darwinSystem`, or `homeManagerConfiguration`)
4. Pass collected modules into the `modules` list, appending host-specific overrides
5. For NixOS/Darwin hosts, integrate home-manager by importing `hmModules` inside the user block
6. If the host needs hardware-specific files, create a subdirectory (e.g., `hosts/myhost/`) and prefix the files with `_`
7. **Update the Module Inventory table below** with the new host entry

### Module Inventory

**IMPORTANT**: This inventory must be kept current. When creating a new module or discovering that a module was added manually, update this table. When exploring the `modules/` tree for any reason and noticing a discrepancy, fix the inventory immediately.

#### Infrastructure Modules

| File | Description | Tiers |
|---|---|---|
| `flake-parts.nix` | Imports flake-parts modules flakeModule and home-manager flakeModule | (flake-parts infrastructure) |
| `options.nix` | Custom option declarations: `username`, `hardware.gpu` | nixos, darwin |
| `systems.nix` | Supported systems list + per-system formatter (`nixpkgs-fmt`) | (flake-parts infrastructure) |
| `state-version.nix` | Default `stateVersion` for NixOS (24.05) and home-manager (24.05) | nixos, homeManager |
| `nixpkgs.nix` | `allowUnfree = true` for NixOS and Darwin | nixos, darwin |

#### Base Modules (`base/`)

| File | Description | Tiers |
|---|---|---|
| `base/git.nix` | Git config: delta, rebase, autoSetupRemote, user identity | homeManager |
| `base/nix.nix` | Lix package manager, flakes, binary caches, GC, registry, nh | nixos, darwin, homeManager |
| `base/shell.nix` | Zsh enable + core CLI tools (bat, eza, fd, fzf, gh, jq, lazygit, ripgrep, shellcheck, tmux) | homeManager |

#### Editor Modules (`editors/`)

| File | Description | Tiers |
|---|---|---|
| `editors/neovim.nix` | Neovim with vi/vim aliases, set as default editor | homeManager |

#### Feature Modules (top-level)

| File | Description | Tiers |
|---|---|---|
| `bluetooth.nix` | Bluetooth with bluez5-experimental and enhanced settings | nixos |
| `boot.nix` | systemd-boot + EFI defaults | nixos |
| `cli.nix` | Dev tools (chezmoi, direnv, devenv, mise, nodejs, ruby, etc.) + program enables | homeManager |
| `developer.nix` | Build tools, GUI apps (ghostty, wezterm), fonts, screenshots, document tools | homeManager |
| `flatpak.nix` | Flatpak via nix-flatpak (Signal) | nixos |
| `gnome.nix` | GNOME desktop + GDM + extensions (system) and dconf dark mode (user) | nixos, homeManager |
| `go.nix` | Go language support | homeManager |
| `graphics.nix` | GPU-conditional graphics drivers (Intel/AMD/NVIDIA) using `config.hardware.gpu` | nixos |
| `gtk.nix` | GTK theming, Bibata cursor, WhiteSur icons and theme | homeManager |
| `locale.nix` | en_US.UTF-8 default locale | nixos |
| `moonlight.nix` | Moonlight game streaming client | nixos |
| `network.nix` | NetworkManager + systemd-resolved with DNS-over-TLS | nixos |
| `steam.nix` | Steam, gamescope, MangoHud, PrismLauncher, gaming sysctl tuning | nixos |
| `sunshine.nix` | Sunshine game streaming server | nixos |
| `users.nix` | User account, zsh shell, SSH keys, 1Password; uses `config.username` | nixos |
| `utils.nix` | Basic system utilities (file, unzip, zip) | nixos |
| `vm.nix` | VM variant config (8GB RAM, 4 cores) | nixos |
| `wayland.nix` | Hyprland + hyprlock + hypridle (system) and Wayland user tools + vicinae (user) | nixos, homeManager |

#### Host/Profile Definitions (`hosts/`)

| File | Description | Output |
|---|---|---|
| `hosts/knid.nix` | Framework 13 7040 AMD laptop (real machine) | `nixosConfigurations.knid` |
| `hosts/knid/_hardware-configuration.nix` | Hardware scan: NVMe, Thunderbolt, KVM-AMD (manually imported by knid.nix) | (not auto-imported) |
| `hosts/knid/_disko.nix` | Disk partitioning: GPT, ESP, btrfs subvolumes (manually imported by knid.nix) | (not auto-imported) |
| `hosts/example-nixos.nix` | Template NixOS host with GRUB + ext4 | `nixosConfigurations.example-nixos` |
| `hosts/example-darwin.nix` | Template nix-darwin host for Apple Silicon | `darwinConfigurations.example-darwin` |
| `hosts/mattr-home.nix` | Standalone home-manager profile for Linux user mattr- | `homeConfigurations.mattr-` |
| `hosts/devcontainer-home.nix` | Standalone home-manager profile for devcontainers (uses `--impure`) | `homeConfigurations.devcontainer` |

### Host Planning

- **`knid`**: Framework 13 7040 AMD laptop -- current test host for all module development
- **Four additional hosts are planned** but not yet defined. We anticipate per-host module selection when these hosts are added.

### Flake Inputs Reference

| Input | Purpose | Follows nixpkgs |
|---|---|---|
| `nixpkgs` | nixos-unstable (shallow clone) | -- |
| `flake-parts` | Modular flake composition framework | yes |
| `import-tree` | Auto-imports all `.nix` files from a directory tree | -- |
| `home-manager` | User environment management | yes |
| `nix-darwin` | macOS system configuration | yes |
| `disko` | Declarative disk partitioning | yes |
| `flatpaks` | nix-flatpak for Flatpak management | -- |
| `hardware` | NixOS hardware quirks (nixos-hardware) | -- |
| `llm-agents` | LLM tools (claude-code, opencode) | -- |
| `minecraft-servers` | Modded Minecraft server support | -- |
| `noctalia` | Noctalia shell | yes |
| `quickshell` | Quickshell compositor widget toolkit | -- |
| `vicinae` | Vicinae home-manager module (Wayland tool) | -- |

### Nix Code Style

- **Formatter**: `nixpkgs-fmt` (run via `nix fmt`)
- **No comments**: Follow the repository-wide comment policy -- do not add comments unless explicitly requested
- **`mkDefault`**: Use for values that hosts should be able to override
- **`let` bindings**: Extract shared configuration used across tiers into `let` bindings at the module level
- **Module arguments**: Use `{ ... }:` for modules that don't need any arguments; use `{ inputs, ... }:`, `{ lib, ... }:`, etc. only when the argument is actually used
- **Attribute naming**: The `<name>` in `flake.modules.<tier>.<name>` should match the feature name (usually the filename without `.nix`)

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
