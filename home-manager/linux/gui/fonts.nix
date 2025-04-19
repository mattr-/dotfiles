{pkgs, ...}: {
  home.packages = with pkgs; [
    iosevka
    nerd-fonts.symbols-only
  ];
}
