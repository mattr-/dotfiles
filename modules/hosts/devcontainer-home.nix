# Devcontainer — impure home-manager collector aspect
#
# Builds a standalone home-manager configuration for devcontainers (GitHub
# Codespaces, VS Code devcontainers, Docker containers). Uses --impure
# evaluation to detect the runtime environment dynamically:
#
#   - Username:       builtins.getEnv "USER"
#   - Home directory: builtins.getEnv "HOME"
#   - Architecture:   builtins.currentSystem
#
# Usage:
#   nix run home-manager -- switch --flake .#devcontainer --impure
{ inputs, config, ... }:
let
  username = builtins.getEnv "USER";
  homeDirectory = builtins.getEnv "HOME";
  system = builtins.currentSystem;
  pkgs = inputs.nixpkgs.legacyPackages.${system};

  # Collect all home-manager feature modules defined across the config
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
in
{
  flake.homeConfigurations."devcontainer" =
    inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = hmModules ++ [
        {
          home = {
            inherit username homeDirectory;
            stateVersion = "24.11";
          };
        }
      ];
    };
}
