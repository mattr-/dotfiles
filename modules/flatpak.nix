{ inputs, ... }:
{
  flake.modules.nixos.flatpak = {
    imports = [ inputs.flatpaks.nixosModules.nix-flatpak ];

    services.flatpak = {
      enable = true;
      packages = [
        "org.signal.Signal"
        "com.microsoft.Edge"
      ];
      overrides = {
        "com.microsoft.Edge".Context = {
          filesystems = [
            "/run/udev:ro"
          ];
        };
      };
    };
  };
}
