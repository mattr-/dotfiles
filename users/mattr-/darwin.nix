{ inputs, pkgs, ... }:

{
  programs.zsh.enable = true;

  users.users.mattr- = {
    home = "/Users/mattr-";
    shell = pkgs.zsh;
  };
}
