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
