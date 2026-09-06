{ ... }:
{
  flake.modules.homeManager.developer = { config, pkgs, lib, ... }:
    let 
      ghostscript = if config.gui.enable then pkgs.ghostscript
        else pkgs.ghostscript_headless;
      emacs = if config.gui.enable then pkgs.emacs-unstable-pgtk
        else pkgs.emacs-unstable-nox;
    in
    {
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
        ghostty.terminfo
      ] ++ lib.optionals config.gui.enable [
        wezterm
        brightnessctl
        anki
        quickshell
        ghostty
      ] ++ [
        ghostscript
        emacs
      ];
    };
}

