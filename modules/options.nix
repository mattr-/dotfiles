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
