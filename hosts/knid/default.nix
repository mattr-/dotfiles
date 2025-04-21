{inputs, outputs, pkgs, ...}: {
  imports = [
    ./hardware-configuration.nix
    ../../system/nixos/core
    ../../system/nixos/core/boot.nix
    ../../system/nixos/network
    ../../system/nixos/network/avahi.nix
    ../../system/nixos/network/tailscale.nix
    ../../system/nixos/services/sound.nix
    ../../system/nixos/services/power.nix
    ../../system/nixos/services/flatpak.nix
    ../../system/nixos/services/ssh.nix
    ../../system/nixos/hardware/bluetooth.nix
    ../../system/nixos/software/xdg.nix
    ../../system/nixos/software/fonts.nix
    ../../system/nixos/services/greetd.nix
    ../../system/nixos/software/hyprland.nix
    ../../system/nixos/software/steam.nix
    ../../system/nixos/users/mattr-.nix
  ];

  networking = {
    hostName = "knid";
  };

  hardware.brillo.enable = true;

  programs.niri.enable = true;


  security = {
    # allow wayland lockers to unlock the screen
    pam.services.hyprlock.text = "auth include login";
  };

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [
      libva
      vaapiVdpau
      libvdpau-va-gl
      amdvlk
      mesa
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [
      vaapiVdpau
      libvdpau-va-gl
      amdvlk
    ];
  };


  system = {
    stateVersion = "24.11";
  };
}
