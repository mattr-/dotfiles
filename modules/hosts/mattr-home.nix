{ inputs, config, pkgs, lib, ... }:
let
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
in
{
  flake.homeConfigurations."mattr-" =
    inputs.home-manager.lib.homeManagerConfiguration {
      modules = hmModules;
    };
}
