{pkgs, ...}: {
  home.packages = with pkgs; [
    unstable.figlet
    unstable.lolcat
  ];

  programs.htop = {
    enable = true;
    package = pkgs.unstable.htop;
  };

}
