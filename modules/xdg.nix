{ ... }:
{
  flake.modules.nixos.xdg = { config, lib, pkgs, ... }: {
    options.dots.xdg.enable = lib.mkEnableOption "XDG portal and user directory configuration";

    config = lib.mkIf config.dots.xdg.enable {
      xdg.portal = {
        enable = true;
        xdgOpenUsePortal = true;
        extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
      };
    };
  };

  flake.modules.homeManager.xdg = {
    xdg.userDirs = {
      enable = true;
      createDirectories = true;
    };
  };
}
