{ pkgs, ... }:
{
  fonts.packages = with pkgs; [
    iosevka
    monaspace
    (nerdfonts.override {fonts = ["NerdFontsSymbolsOnly"];})
  ];
}
