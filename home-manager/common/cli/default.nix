{pkgs, ...}: {
  home.packages = with pkgs; [
    chezmoi
    fzy
    gitui
    gitFull
    git-lfs
    gh
    luajitPackages.luarocks
    neovim
  ];

  programs.bat = {
    enable = true;
    package = pkgs.bat;
  };

  programs.eza = {
    enable = true;
    package = pkgs.eza;
  };

  programs.fzf = {
    enable = true;
    package = pkgs.fzf;
  };

  programs.jq = {
    enable = true;
    package = pkgs.jq;
  };

  programs.ripgrep = {
    enable = true;
    package = pkgs.ripgrep;
  };

  programs.fd = {
    enable = true;
    package = pkgs.fd;
  };

  programs.lazygit = {
    enable = true;
    package = pkgs.lazygit;
  };
}
