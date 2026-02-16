{ inputs, ... }:
{
  flake.modules.nixos.flatpak = {
    imports = [ inputs.flatpaks.nixosModules.nix-flatpak ];

    services.flatpak = {
      enable = true;
      packages = [ "org.signal.Signal" ];
    };
  };
}
