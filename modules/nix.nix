{ inputs, ... }:
let
  sharedNixConfig = { pkgs, lib, ... }: {
    nix.package = pkgs.lixPackageSets.stable.lix;

    nix.settings = {
      experimental-features = [ "nix-command" "flakes" ];

      extra-substituters = [ "https://cache.lix.systems" ];
      extra-trusted-public-keys = [
        "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
      ];

      trusted-users = [ "root" "@wheel" "mattr-" ];
      auto-optimise-store = lib.mkDefault true;
      warn-dirty = false;
      flake-registry = "";
    };

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

    nix.registry = lib.mapAttrs (_: flake: { inherit flake; })
      (lib.filterAttrs (_: lib.isType "flake") inputs);
    nix.nixPath = lib.mapAttrsToList (n: _: "${n}=flake:${n}")
      (lib.filterAttrs (_: lib.isType "flake") inputs);

    programs.nh.enable = true;

    system.activationScripts.system-report-changes = ''
      PATH=$PATH:${lib.makeBinPath [ pkgs.nvd pkgs.nix ]}
      if [ $(ls -d1v /nix/var/nix/profiles/system-*-link 2>/dev/null | wc -l) -lt 2 ]
      then
        echo "Skipping system change report..."
      else
        nvd diff $(ls -d1v /nix/var/nix/profiles/system-*-link | tail -n2)
      fi
    '';
  };

  flake.modules.darwin.nix = sharedNixConfig;

  flake.modules.homeManager.nix = { ... }: {
    programs.nh.enable = true;
  };
}
