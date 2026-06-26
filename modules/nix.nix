{ inputs, ... }:
let
  # Shared nix settings used by both NixOS and nix-darwin
  sharedNixConfig = { pkgs, lib, ... }: {
    nix = {
      package = pkgs.lixPackageSets.stable.lix;

      settings = {
        experimental-features = [ "nix-command" "flakes" ];

        # Lix binary cache
        extra-substituters = [ "https://cache.lix.systems" ];
        extra-trusted-public-keys = [
          "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
        ];

        trusted-users = [ "root" "@wheel" "mattr-" ];
        auto-optimise-store = lib.mkDefault true;
        warn-dirty = false;
        flake-registry = "";
      };

      # Periodic garbage collection
      gc = {
        automatic = true;
        options = "--delete-older-than 30d";
      };
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
