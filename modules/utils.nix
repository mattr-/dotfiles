{ ... }:
{
  flake.modules.nixos.utils = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      file
      unzip
      zip
    ];
  };
}
