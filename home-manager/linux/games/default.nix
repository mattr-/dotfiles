{ pkgs, ...}:
{
  home.packages = with pkgs; [
    unstable.prismlauncher
  ];
}
