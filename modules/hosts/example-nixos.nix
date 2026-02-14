# Example NixOS host — collector aspect
#
# Composes feature modules into a NixOS system configuration.
# Rename this file to match your hostname and update the attribute name
# in nixosConfigurations accordingly.
#
# Usage:
#   sudo nixos-rebuild switch --flake .#example-nixos
{ inputs, config, ... }:
let
  # Collect all NixOS feature modules (e.g., nix.nix contributes flake.modules.nixos.nix)
  nixosModules = builtins.attrValues (config.flake.modules.nixos or { });

  # Collect all home-manager feature modules for the user
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
in
{
  flake.nixosConfigurations."example-nixos" = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = nixosModules ++ [
      inputs.home-manager.nixosModules.home-manager
      {
        nixpkgs.hostPlatform = "x86_64-linux";

        # Set the system state version
        system.stateVersion = "24.11";

        # User account
        users.users."mattr-" = {
          isNormalUser = true;
          extraGroups = [ "wheel" "networkmanager" ];
          home = "/home/mattr-";
        };

        # Home-manager integration as a NixOS module
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
