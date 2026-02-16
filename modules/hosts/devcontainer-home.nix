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
