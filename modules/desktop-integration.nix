{ ... }:
{
  flake.modules.homeManager.desktop-integration = { config, lib, pkgs, ... }:
    lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
      home.pointerCursor = lib.mkIf config.gui.enable {
        enable = true;
        package = pkgs.bibata-cursors;
        name = "Bibata-Original-Ice";
        size = 20;
        gtk.enable = true;
        x11.enable = true;
      };

      gtk = lib.mkIf config.gui.enable {
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

      xdg.userDirs = {
        enable = true;
        createDirectories = true;
        setSessionVariables = true;
      };

      xdg.mimeApps = {
        enable = true;
        defaultApplications = {
          "text/html" = "firefox.desktop";
          "x-scheme-handler/http" = "firefox.desktop";
          "x-scheme-handler/https" = "firefox.desktop";
          "x-scheme-handler/about" = "firefox.desktop";
          "x-scheme-handler/unknown" = "firefox.desktop";
          "application/xhtml+xml" = "firefox.desktop";

          "application/pdf" = "org.gnome.Papers.desktop";
          "application/x-bzpdf" = "org.gnome.Papers.desktop";
          "application/x-ext-pdf" = "org.gnome.Papers.desktop";
          "application/x-gzpdf" = "org.gnome.Papers.desktop";
          "application/x-xzpdf" = "org.gnome.Papers.desktop";
          "application/illustrator" = "org.gnome.Papers.desktop";
          "image/vnd.djvu" = "org.gnome.Papers.desktop";
          "image/vnd.djvu+multipage" = "org.gnome.Papers.desktop";
          "image/tiff" = "org.gnome.Papers.desktop";
          "application/vnd.comicbook-rar" = "org.gnome.Papers.desktop";
          "application/vnd.comicbook+zip" = "org.gnome.Papers.desktop";
          "application/x-cb7" = "org.gnome.Papers.desktop";
          "application/x-cbr" = "org.gnome.Papers.desktop";
          "application/x-cbt" = "org.gnome.Papers.desktop";
          "application/x-cbz" = "org.gnome.Papers.desktop";

          "image/png" = "org.gnome.Loupe.desktop";
          "image/jpeg" = "org.gnome.Loupe.desktop";
          "image/gif" = "org.gnome.Loupe.desktop";
          "image/webp" = "org.gnome.Loupe.desktop";
          "image/svg+xml" = "org.gnome.Loupe.desktop";
          "image/bmp" = "org.gnome.Loupe.desktop";
          "image/avif" = "org.gnome.Loupe.desktop";
          "image/heic" = "org.gnome.Loupe.desktop";
          "image/jxl" = "org.gnome.Loupe.desktop";

          "video/mp4" = "org.gnome.Showtime.desktop";
          "video/webm" = "org.gnome.Showtime.desktop";
          "video/x-matroska" = "org.gnome.Showtime.desktop";
          "video/ogg" = "org.gnome.Showtime.desktop";
          "video/quicktime" = "org.gnome.Showtime.desktop";
          "video/x-msvideo" = "org.gnome.Showtime.desktop";
          "video/mpeg" = "org.gnome.Showtime.desktop";
          "video/x-flv" = "org.gnome.Showtime.desktop";
          "video/3gpp" = "org.gnome.Showtime.desktop";

          "audio/mpeg" = "org.gnome.Decibels.desktop";
          "audio/x-flac" = "org.gnome.Decibels.desktop";
          "audio/x-vorbis+ogg" = "org.gnome.Decibels.desktop";
          "audio/x-opus+ogg" = "org.gnome.Decibels.desktop";
          "audio/wav" = "org.gnome.Decibels.desktop";
          "audio/x-m4a" = "org.gnome.Decibels.desktop";
          "audio/x-aac" = "org.gnome.Decibels.desktop";
          "audio/x-wavpack" = "org.gnome.Decibels.desktop";

          "text/plain" = "org.gnome.TextEditor.desktop";
          "application/x-zerosize" = "org.gnome.TextEditor.desktop";

          "inode/directory" = "org.gnome.Nautilus.desktop";
        };
      };
    };
}
