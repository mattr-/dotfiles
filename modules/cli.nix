{ ... }:
{
  flake.modules.nixos.cli = { pkgs, ... }: {
    environment.systemPackages = [ pkgs.vim ];
  };

  flake.modules.darwin.cli = { pkgs, ... }: {
    environment.systemPackages = [ pkgs.vim ];
  };

  flake.modules.homeManager.cli = { pkgs, ... }: {
    programs.zsh.enable = true;

    home.packages = with pkgs; [
      chezmoi
      fzy
      gitui
      git-lfs
      gh
      jujutsu
      mr
      luajitPackages.luarocks
      direnv
      devenv
      mise
      nodejs
      ruby
    ];

    programs.bat.enable = true;
    programs.eza.enable = true;
    programs.fzf.enable = true;
    programs.jq.enable = true;
    programs.ripgrep.enable = true;
    programs.fd.enable = true;
    programs.lazygit.enable = true;
  };
}

