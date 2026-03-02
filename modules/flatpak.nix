{ inputs, ... }:
{
  flake.modules.nixos.flatpak = { config, lib, ... }: {
    imports = [ inputs.flatpaks.nixosModules.nix-flatpak ];

    config = lib.mkIf config.dots.flatpak.enable {
      services.flatpak = {
        enable = true;
        packages = [ "org.signal.Signal" ];
      };
    };
  };
}
