{ pkgs, ... }: {
  home.packages = with pkgs; [
    unstable.wezterm
    unstable.ghostty
    unstable.vivaldi
  ];
}
