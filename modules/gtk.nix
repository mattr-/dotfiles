{ ... }:
{
  flake.modules.homeManager.gtk = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.gui.enable {
      home.pointerCursor = {
        package = pkgs.bibata-cursors;
        name = "Bibata-Original-Ice";
        size = 20;
        gtk.enable = true;
        x11.enable = true;
      };

      gtk = {
        enable = true;
        gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";

        iconTheme = {
          name = "WhiteSur";
          package = pkgs.whitesur-icon-theme.override {
            boldPanelIcons = true;
            alternativeIcons = true;
          };
        };

        theme = {
          package = pkgs.whitesur-gtk-theme;
          name = "WhiteSur-Dark";
        };

        gtk4.theme = config.gtk.theme;
      };
    };
  };
}
