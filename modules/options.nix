{ lib, ... }:
{
  flake.modules.nixos.options = {
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

      keyd.enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether to enable keyd key remapping";
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
