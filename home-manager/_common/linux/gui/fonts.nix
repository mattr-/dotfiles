{ pkgs, ... }:
{
  home.packages = with pkgs; [
    unstable.iosevka
  ];
}
