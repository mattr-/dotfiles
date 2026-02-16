# Shell configuration — home-manager feature aspect
#
# Configures ZSH and common CLI tools. This is a home-manager deferred module
# that can be composed into any host or standalone home-manager configuration.
{ ... }:
{
  flake.modules.homeManager.shell = { pkgs, ... }: {
    # Let home-manager manage the shell
    programs.zsh = {
      enable = true;
    };

    # Common CLI tools managed by Nix
    home.packages = with pkgs; [
      bat
      eza
      fd
      fzf
      gh
      jq
      lazygit
      ripgrep
      shellcheck
      tmux
    ];
  };
}
