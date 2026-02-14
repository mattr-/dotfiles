# mattr- — standalone home-manager collector aspect
#
# Builds a standalone home-manager configuration for personal Linux machines
# where the username is always mattr-. For devcontainers with dynamic usernames,
# see devcontainer.nix instead.
#
# Usage:
#   nix run home-manager -- switch --flake .#mattr-
{ inputs, config, ... }:
let
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
in
{
  flake.homeConfigurations."mattr-" =
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
      modules = hmModules ++ [
        {
          home = {
            username = "mattr-";
            homeDirectory = "/home/mattr-";
            stateVersion = "24.11";
          };
        }
      ];
    };
}
