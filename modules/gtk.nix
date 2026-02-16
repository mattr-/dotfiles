{ ... }:
{
  flake.modules.homeManager.gtk = { config, pkgs, ... }: {
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
    };
  };
}
