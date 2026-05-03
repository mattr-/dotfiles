{ ... }:
{
  flake.modules.nixos.utils = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      file
      pciutils
      unzip
      usbutils
      zip
    ];
  };
}
