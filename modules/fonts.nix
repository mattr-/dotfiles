{ ... }:
{
  flake.modules.nixos.fonts = { pkgs, ... }: {
    fonts = {
      packages = with pkgs; [
        noto-fonts
        noto-fonts-cjk-sans
        noto-fonts-color-emoji

        nerd-fonts.symbols-only

        material-symbols
        inter
        fira-code
      ];

      enableDefaultPackages = false;

      fontconfig = {
        defaultFonts = {
          serif = [
            "Noto Serif"
            "Noto Sans CJK"
            "Symbols Nerd Font Mono"
          ];
          sansSerif = [
            "Noto Sans"
            "Noto Sans CJK"
            "Symbols Nerd Font Mono"
          ];
          monospace = [
            "Noto Sans Mono"
            "Noto Sans CJK"
            "Symbols Nerd Font Mono"
          ];
          emoji = [
            "Noto Color Emoji"
            "Symbols Nerd Font Mono"
          ];
        };
      };

      fontDir = {
        enable = true;
      };
    };
  };
}
