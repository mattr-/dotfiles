{ pkgs, ... }: {
  home.packages = with pkgs; [
    wezterm
    ghostty
  ];
}
