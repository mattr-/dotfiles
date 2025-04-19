{pkgs, ...}: {
  home.packages = with pkgs; [
    figlet
    lolcat
  ];

  programs.htop = {
    enable = true;
    package = pkgs.htop;
  };

}
