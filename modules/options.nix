{ lib, ... }:
{
  flake.modules.nixos.options = { config, ... }: {
    options = {
      username = lib.mkOption {
        type = lib.types.str;
        default = "mattr-";
        description = "Primary username for this configuration";
      };

      hardware.gpu = lib.mkOption {
        type = lib.types.enum [ "intel" "nvidia" "amd" ];
        default = "intel";
        description = "GPU type for graphics configuration";
      };

      dots.graphical.available = lib.mkOption {
        type = lib.types.bool;
        default = config.dots.wayland.enable || config.dots.gnome.enable;
        readOnly = true;
        internal = true;
        description = "Whether a graphical session is available";
      };
    };
  };

  flake.modules.darwin.options = {
    options = {
      username = lib.mkOption {
        type = lib.types.str;
        default = "mattr-";
        description = "Primary username for this configuration";
      };
    };
  };
}
