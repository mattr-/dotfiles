# Neovim — home-manager feature aspect
#
# Installs neovim via home-manager. Configuration is managed externally
# (e.g., via chezmoi or XDG config files), so this module only ensures
# the neovim package is available.
{ ... }:
{
  flake.modules.homeManager.neovim = { pkgs, ... }: {
    programs.neovim = {
      enable = true;
      defaultEditor = true;
      vimAlias = true;
      viAlias = true;
    };
  };
}
