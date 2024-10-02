{pkgs, ...}: {
  home.packages = with pkgs; [
    gnumake
    automake
    autoconf
    gcc
    libgcc
  ];

  imports = [
    ../../common/dev/mise.nix
  ];
}
