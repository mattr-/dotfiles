{pkgs, ...}: {
  home.packages = with pkgs; [
    unstable.chezmoi
    unstable.fzy
    unstable.gitui
    unstable.gitFull
    unstable.gh
    unstable.neovim
  ];

  programs.bat = {
    enable = true;
    package = pkgs.unstable.bat;
  };

  programs.eza = {
    enable = true;
    package = pkgs.unstable.eza;
  };

  programs.fzf = {
    enable = true;
    package = pkgs.unstable.fzf;
  };

  programs.jq = {
    enable = true;
    package = pkgs.unstable.jq;
  };

  programs.ripgrep = {
    enable = true;
    package = pkgs.unstable.ripgrep;
  };

  programs.fd = {
    enable = true;
    package = pkgs.unstable.fd;
  };

  programs.lazygit = {
    enable = true;
    package = pkgs.unstable.lazygit;
  };
}
