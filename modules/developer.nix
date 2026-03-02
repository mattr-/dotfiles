{ ... }:
{
  flake.modules.homeManager.developer = { pkgs, ... }: {
    home.packages = with pkgs; [
      gnumake
      automake
      autoconf
      gcc
      libgcc
      ghostty
      wezterm
      iosevka
      nerd-fonts.symbols-only
      hyprshot
      brightnessctl
      imagemagick
      ghostscript
      tectonic
      mermaid-cli
      anki
      quickshell
    ];

    home.sessionVariables = {
      XDG_ICON_DIR = "${pkgs.whitesur-icon-theme}/share/icons/WhiteSur";
    };

    programs.home-manager.enable = true;

    systemd.user.startServices = "sd-switch";
  };
}
