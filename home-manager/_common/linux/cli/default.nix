{ pkgs, ... }:
{
  home.packages = with pkgs; [
    unstable.neovim
    unstable.gitui
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

  programs.git = {
    enable = true;
    package = pkgs.unstable.gitFull;
  };

  programs.gh = {
    enable = true;
    package = pkgs.unstable.gh;
    gitCredentialHelper = {
      enable = true;
    };
  };

}
