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
