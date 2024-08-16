{inputs, pkgs, ...}:
{
  imports = [
    ./hardware-configuration.nix
    ../_global/linux
    ../_global/linux/systemd-boot.nix
    ../../users/mattr-/nixos.nix
    ../../modules/nixos/vmware-guest-aarch64-compat.nix
  ];

  networking = {
    hostName = "violet";
  };

  # Too many things are marked as broken on aarch64. :(
  nixpkgs.config.allowUnsupportedSystem = true;

  # Don't require password for sudo
  security.sudo.wheelNeedsPassword = false;

  # Allow docker containers inside the dev vm
  virtualisation.docker.enable = true;

  # Set up vmware guest tools
  # VMWare and Parallels both only support this being 0
  boot.loader.systemd-boot.consoleMode = "0";
  virtualisation.vmware.guest.enable = true;
  virtualisation.vmware.guest.headless = true;

  # Hack in some graphics
  services.xserver = {
    enable = true;
    xkb.layout = "us";
    dpi = 110;

    desktopManager = {
      xterm.enable = false;
      wallpaper.mode = "fill";
    };

    displayManager = {
      lightdm.enable = true;

      sessionCommands = ''
        ${pkgs.xorg.xset}/bin/xset r rate 200 40
        '';
    };

    windowManager = {
      i3.enable = true;
    };
  };

  services.displayManager.defaultSession = "none+i3";


  system.stateVersion = "24.05";
}
