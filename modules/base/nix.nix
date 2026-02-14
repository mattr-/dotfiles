# Nix / Lix configuration — multi-context aspect (nixos + darwin)
#
# Configures Lix as the Nix implementation, enables flakes and the nix command,
# and sets up common nix daemon settings. This module defines both NixOS and
# nix-darwin deferred modules so the same nix settings apply on either platform.
{ inputs, ... }:
let
  # Shared nix settings used by both NixOS and nix-darwin
  sharedNixConfig = { pkgs, ... }: {
    # Use Lix as the Nix implementation
    nix.package = pkgs.lixPackageSets.stable.lix;

    nix.settings = {
      # Enable flakes and the nix command
      experimental-features = [ "nix-command" "flakes" ];

      # Lix binary cache
      extra-substituters = [ "https://cache.lix.systems" ];
      extra-trusted-public-keys = [
        "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
      ];

      # Allow the primary user to manage the nix store
      trusted-users = [ "root" "mattr-" ];
    };

    # Periodic garbage collection
    nix.gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
  };
in
{
  flake.modules.nixos.nix = sharedNixConfig;
  flake.modules.darwin.nix = sharedNixConfig;
}
