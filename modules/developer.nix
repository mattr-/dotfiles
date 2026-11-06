{ ... }:
{
  flake.modules.nixos.developer = { config, lib, pkgs, ... }:
    let
      ghostscript = if config.gui.enable then pkgs.ghostscript
        else pkgs.ghostscript_headless;
      emacs = if config.gui.enable then pkgs.emacs-unstable-pgtk
        else pkgs.emacs-unstable-nox;
    in
    {
      environment.systemPackages = [
        ghostscript
        emacs
      ] ++ lib.optionals config.gui.enable (with pkgs; [
        ghostty
        wezterm
        anki
        quickshell
      ]);
    };

  flake.modules.homeManager.developer = { config, pkgs, lib, ... }: {
    home.packages = with pkgs; [
      gnumake
      automake
      autoconf
      gcc
      libgcc

      shellcheck

      imagemagick

      tectonic
      mermaid-cli
      ast-grep
      tree-sitter
    ] ++ lib.optionals config.gui.enable [
      brightnessctl
    ];
  };
}

