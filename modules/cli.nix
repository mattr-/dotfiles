{ ... }:
{
  flake.modules.homeManager.cli = { pkgs, ... }: {
    home.packages = with pkgs; [
      # Version control
      chezmoi
      gitui
      git-lfs
      jujutsu
      mr

      # Development tools
      direnv
      devenv
      mise
      nodejs
      ruby
      luajitPackages.luarocks

      # CLI utilities
      fzy
    ];

    # Program configurations
    programs.bat.enable = true;
    programs.eza.enable = true;
    programs.fzf.enable = true;
    programs.jq.enable = true;
    programs.ripgrep.enable = true;
    programs.fd.enable = true;
    programs.lazygit.enable = true;
  };
}

