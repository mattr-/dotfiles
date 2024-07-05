{
  lib,
  outputs,
  pkgs,
  ...
}:
{
  imports = [
    ./nix.nix
    ./openssh.nix
    ./time-and-locale.nix
  ];

  # Allow microcode updates for CPUs
  hardware.enableRedistributableFirmware = true;

  # Use NetworkManager for networking
  networking.networkmanager.enable = true;

  # Provide vim, curl, and git on every linux system
  # because I use these all the time.
  environment.systemPackages = with pkgs; [
    vim
    curl
    git
  ];
}
