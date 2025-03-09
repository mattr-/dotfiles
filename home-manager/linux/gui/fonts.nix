{pkgs, ...}: {
  home.packages = with pkgs; [
    unstable.iosevka
    unstable.nerd-fonts.symbols-only
  ];
}
