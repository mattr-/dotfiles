{ inputs, ... }:
let
  # Shared nix settings used by both NixOS and nix-darwin
  sharedNixConfig = { pkgs, lib, ... }: {
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
      trusted-users = [ "root" "@wheel" "mattr-" ];

      # Optimize store automatically
      auto-optimise-store = lib.mkDefault true;

      # Don't warn about dirty git trees
      warn-dirty = false;

      # Disable global registry
      flake-registry = "";
    };

    # Periodic garbage collection
    nix.gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
  };
in
{
  flake.modules.nixos.nix = { config, lib, pkgs, inputs, ... }: {
    imports = [ (sharedNixConfig { inherit pkgs lib; }) ];

    nix.settings.nix-path = config.nix.nixPath;
    nix.channel.enable = false;

    # Map flake inputs to registry and nixPath
    nix.registry = lib.mapAttrs (_: flake: { inherit flake; })
      (lib.filterAttrs (_: lib.isType "flake") inputs);
    nix.nixPath = lib.mapAttrsToList (n: _: "${n}=flake:${n}")
      (lib.filterAttrs (_: lib.isType "flake") inputs);
  };

  flake.modules.darwin.nix = sharedNixConfig;

  flake.modules.homeManager.nix = { pkgs, lib, ... }: {
    # Enable nh for home-manager (can still build NixOS systems)
    programs.nh.enable = true;
  };
}
