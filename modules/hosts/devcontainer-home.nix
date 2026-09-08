{ inputs, config, ... }:
let
  system = "x86_64-linux";
  username = builtins.getEnv "USER";
  homeDirectory = builtins.getEnv "HOME";

  # Collect all home-manager feature modules defined across the config
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
  pkgs = import inputs.nixpkgs {
    inherit system;
    config.allowUnfree = true;
    overlays = [ config.flake.overlays.default ];
  };
in
{
  flake.homeConfigurations."devcontainer" =
    inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = hmModules ++ [
        {
          gui.enable = false;
          home = {
            inherit username homeDirectory;
            stateVersion = "24.11";
          };
        }
      ];
    };
}
