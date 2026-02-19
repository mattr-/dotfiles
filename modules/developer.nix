{ ... }:
{
  flake.modules.homeManager.developer = { pkgs, ... }: {
    home.packages = with pkgs; [
      # Build tools
      gnumake
      automake
      autoconf
      gcc
      libgcc

      # Applications
      ghostty
      wezterm

      # Fonts
      iosevka
      nerd-fonts.symbols-only

      # Utilities
      hyprshot
      brightnessctl
      imagemagick
      ghostscript

      # Document tools
      tectonic
      mermaid-cli
      anki

      # Compositor
      quickshell
    ];
  };
}

