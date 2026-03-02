{ lib, ... }:
let
  mkDotEnable = description: lib.mkEnableOption description;
in
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

      dots = {
        audio.enable = mkDotEnable "PipeWire audio stack";
        bluetooth.enable = mkDotEnable "Bluetooth support";
        flatpak.enable = mkDotEnable "Flatpak package management";
        fonts.enable = mkDotEnable "System font configuration";
        firefox.enable = mkDotEnable "Firefox browser";
        gaming.enable = mkDotEnable "Gaming support (Steam, gamescope, etc.)";
        gnome.enable = mkDotEnable "GNOME desktop environment";
        graphics.enable = mkDotEnable "GPU graphics drivers";
        keyd.enable = mkDotEnable "Keyboard remapping with keyd";
        moonlight.enable = mkDotEnable "Moonlight game streaming client";
        power.enable = mkDotEnable "Power management";
        sunshine.enable = mkDotEnable "Sunshine game streaming server";
        wayland.enable = mkDotEnable "Wayland compositor support";
        xdg.enable = mkDotEnable "XDG portal and user directory configuration";
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
