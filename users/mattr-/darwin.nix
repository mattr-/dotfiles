{ inputs, pkgs, ... }:

{
  users.users.mattr- = {
    home = "/Users/mattr-";
    shell = pkgs.zsh;
  };
}
