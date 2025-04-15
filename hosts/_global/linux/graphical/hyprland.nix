{
  programs.hyprland = {
    enable = true;
    withUWSM = true; # recommended for most users
    xwayland.enable = false; # Xwayland can be disabled.
  };

  # Electron apps should use Wayland
  environment.sessionVariables.NIXOS_OZONE_WL = "1";
}
