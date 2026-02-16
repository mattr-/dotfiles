{ inputs, config, ... }:
let
  # Collect all darwin feature modules (e.g., nix.nix contributes flake.modules.darwin.nix)
  darwinModules = builtins.attrValues (config.flake.modules.darwin or { });

  # Collect all home-manager feature modules for the user
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
in
{
  flake.darwinConfigurations."example-darwin" = inputs.nix-darwin.lib.darwinSystem {
    system = "aarch64-darwin";
    modules = darwinModules ++ [
      inputs.home-manager.darwinModules.home-manager
      {
        # System-level settings
        nixpkgs.hostPlatform = "aarch64-darwin";

        # Set the system state version
        system.stateVersion = 6;

        # User account
        users.users."mattr-" = {
          home = "/Users/mattr-";
        };

        # Home-manager integration as a nix-darwin module
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          users."mattr-" = { ... }: {
            imports = hmModules;
            home.stateVersion = "24.11";
          };
        };
      }
    ];
  };
}
