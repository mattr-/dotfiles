{ inputs, ... }:
{
  flake.modules.nixos.wayland = { pkgs, ... }: {
    programs.hyprland = {
      enable = true;
      withUWSM = true;
      xwayland.enable = true;
    };

    programs.hyprlock.enable = true;
    services.hypridle.enable = true;

    environment.sessionVariables.NIXOS_OZONE_WL = "1";

    environment.systemPackages = with pkgs; [
      xwayland-satellite
    ];

    security.pam.services.hyprlock.text = "auth include login";
  };

  flake.modules.homeManager.wayland = { pkgs, ... }: {
    imports = [ inputs.vicinae.homeManagerModules.default ];

    home.packages = with pkgs; [
      cliphist
      dunst
      swayosd
      swww
      waybar
      wev
      wofi
      wl-clipboard
      wlsunset
    ];

    services.vicinae = {
      enable = true;
      autoStart = true;
    };
  };
}

