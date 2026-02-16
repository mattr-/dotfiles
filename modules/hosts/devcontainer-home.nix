{ inputs, config, ... }:
let
  username = builtins.getEnv "USER";
  homeDirectory = builtins.getEnv "HOME";

  # Collect all home-manager feature modules defined across the config
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
in
{
  flake.homeConfigurations."devcontainer" =
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
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
