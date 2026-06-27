{ ... }:
{
  flake.modules.homeManager.users = { config, ... }: {

    home = {
      username = ${config.user.name};
      homeDirectory = ${config.user.home};
      stateVersion = "24.11";
    }

    programs.home-manager.enable = true;
    systemd.user.startServices = "sd-switch";

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

  flake.modules.nixos.users = { pkgs, config, ... }: {
    programs.zsh.enable = true;

    users.users.${config.username} = {
      isNormalUser = true;
      shell = pkgs.zsh;
      initialPassword = "1";
      extraGroups = [ "wheel" "networkmanager" "video" ];
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDtwYzNNb4WCrOSnIA6oC6LO4pagTo4jGzcR4W3fXCCfaRc14cZiuc3ZmHCMzGKTS3xo63Zz2cvFa8ZFF3IXgjXXOhF3l89xgiQZ8NgIemqNco5C8GPXzkWyvQF3yKXKWLSC1R8aYnI1gakpVA5iJ7CVwvZdTRVP01wK6fJDTM2N8uUIA2JuzYrDH6uH1t4RNBaGovG9G3yfwoYyR2w2UCXrQLGzOHFDSn/y+B6ClDuFngHazktaE4Fnh05AnKw+EQgii61b906XViWW0rcqQktcjiVlIrp8AgIK9yhi5ED0ZaIflFkWh+5KDH4tDMTymT47Cn+GlLTj6TZve+ywxRDr1HNIJTIvBICTFQAZjKL9ep5auKK1V8dmNCHAxfAfwCWqEnuimqupgR2tqZtY0fmZccboZAaac9cvJ0Na130FxTP5ThLusdDXrteKstx0etowL23KBGO6vZobaJkTG8QYjHnGhcvr8u3g7HeYRMWDs//9fgoaIQUv1D9Epe79y9Y0odUPdDhMt3ZerWuKoBfC7xJo9IWl4eHarrXf3yKk5JbAIfVXItn8K94WMpRD1eFdVMpufcnXbD1CnBEz52dL2a+hoH9O9CXpMxgRiQK05zQiYi0ybc+iHKEEyF9Vl1URrfiuzAZ5LpAPd73XcijWJViNgLi08ZtO+RkB0CLBQ=="
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJoNNZg/ZDdwjVukEuSjFsehBSlBXPqRKTlrDYGxUyZw"
      ];
    };

    # 1Password integration
    programs._1password.enable = true;
    programs._1password-gui = {
      enable = true;
      polkitPolicyOwners = [ config.username ];
    };
  };
}
