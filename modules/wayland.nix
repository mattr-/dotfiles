{ ... }:
{
  flake.modules.nixos.wayland = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.gui.enable {
      environment.sessionVariables.NIXOS_OZONE_WL = "1";

      environment.systemPackages = with pkgs; [
        awww
        cliphist
        swayosd
        waybar
        wev
        wofi
        wl-clipboard
        wlsunset
        xwayland-satellite
      ];

      security.pam.services.hyprlock.text = "auth include login";
    };
  };
}
