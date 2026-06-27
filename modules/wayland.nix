{ ... }:
{
  flake.modules.nixos.wayland = { pkgs, ... }: {
    environment.sessionVariables.NIXOS_OZONE_WL = "1";

    environment.systemPackages = with pkgs; [
      xwayland-satellite
    ];

    security.pam.services.hyprlock.text = "auth include login";
  };

  flake.modules.homeManager.wayland = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.gui.enable {
      home.packages = with pkgs; [
        cliphist
        dunst
        swayosd
        awww
        waybar
        wev
        wofi
        wl-clipboard
        wlsunset
      ];

    };
  };
}

