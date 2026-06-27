{ lib, ... }:
{
  flake.modules.nixos.options = {}: {
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

  flake.modules.darwin.options = {}: {
    options = {
      username = lib.mkOption {
        type = lib.types.str;
        default = "mattr-";
        description = "Primary username for this configuration";
      };
    };
  };

  flake.modules.homeManager.options = { pkgs, ... }: {
    options = {
      gui.enable = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to include GUI applications and configuration";
      };

      user.name = lib.mkOption {
        type = lib.types.str;
        default = "mattr-";
        description = "Primary username for this configuration";
      };

      user.home = lib.mkOption {
        type = lib.types.str;
        default = (if pkgs.stdenv.isDarwin
          then "/Users/mattr-"
          else "/home/mattr-");
        description = "The home directory for the user";
      };
    };
  };
}
