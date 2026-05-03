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

  flake.modules.homeManager.utils = { pkgs, ... }: {
    home.packages = with pkgs; [
      bc
    ];
  };
}
