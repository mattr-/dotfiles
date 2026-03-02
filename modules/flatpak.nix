{ inputs, ... }:
{
  flake.modules.nixos.flatpak = { config, lib, ... }: {
    imports = [ inputs.flatpaks.nixosModules.nix-flatpak ];

    options.dots.flatpak.enable = lib.mkEnableOption "Flatpak package management";

    config = lib.mkIf config.dots.flatpak.enable {
      services.flatpak = {
        enable = true;
        packages = [ "org.signal.Signal" ];
      };
    };
  };
}
