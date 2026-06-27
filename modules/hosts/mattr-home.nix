{ inputs, config, pkgs, lib, ... }:
let
  system = "x86_64-linux";
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
  pkgs = import inputs.nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = [ config.flake.overlays.default ];
  };
in
{
  flake.homeConfigurations."mattr-" =
    inputs.home-manager.lib.homeManagerConfiguration {
      modules = hmModules;
    };
}
