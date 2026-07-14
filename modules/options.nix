{ lib, ... }:
let
  inherit (lib)
    mkOption
  ;
  usernameOption = mkOption {
    type = lib.types.str;
    default = "mattr-";
    description = "Primary username for this configuration";
  };

  guiEnableOption = mkOption {
    type = lib.types.bool;
    default = true;
    description = "Whether to include GUI applications and configuration";
  };
in

{
  flake.modules.nixos.options = {
    options = {
      username = usernameOption;

      hardware.gpu = mkOption {
        type = lib.types.enum [ "intel" "nvidia" "amd" ];
        default = "intel";
        description = "GPU type for graphics configuration";
      };

      keyd.enable = mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether to enable keyd key remapping";
      };

      gui.enable = guiEnableOption;
    };
  };

  flake.modules.darwin.options = {
    options = {
      username = usernameOption;
    };
  };

  flake.modules.homeManager.options = {
    options = {
      gui.enable = guiEnableOption;
    };
  };
}
