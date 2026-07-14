{ inputs, ... }:
{
  flake.modules.nixos.flatpak = { lib, config, ... }: {
    imports = [ inputs.flatpaks.nixosModules.nix-flatpak ];

    config = lib.mkIf config.gui.enable {
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
          "org.signal.Signal".Environment = {
            TZ = config.time.timeZone;
          };
        };
      };
    };
  };
}
