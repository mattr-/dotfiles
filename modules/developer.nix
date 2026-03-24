{ ... }:
{
  flake.modules.homeManager.developer = { config, pkgs, lib, ... }: {
    home.packages = with pkgs; [
      gnumake
      automake
      autoconf
      gcc
      libgcc

      shellcheck

      imagemagick
      ghostscript

      tectonic
      mermaid-cli
    ] ++ lib.optionals config.gui.enable [
      ghostty
      wezterm
      hyprshot
      brightnessctl
      anki
      quickshell
    ];
  };
}

