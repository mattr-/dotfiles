{ lib, outputs, pkgs, ... }:
{
  imports = [
    ./nix.nix
    ./homebrew.nix
    ./macos.nix
    ./fonts.nix
  ];
}
