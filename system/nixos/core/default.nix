
{
  lib,
  outputs,
  pkgs,
  ...
}: {
  imports = [
    ./nix.nix
    ./sysctl.nix
  ];

  # Allow microcode updates for CPUs
  hardware.enableRedistributableFirmware = true;

  # Default locale settings which can be overridden on a per system
  # basis
  i18n = {
    defaultLocale = lib.mkDefault "en_US.UTF-8";
  };
  time.timeZone = lib.mkDefault "America/Chicago";

  # Configure sudo
  security.sudo = {
    enable = true;
    keepTerminfo = true;
    execWheelOnly = true;
    wheelNeedsPassword = false;
  };

  # Provide vim, curl, and git on every linux system
  # because I use these all the time.
  environment.systemPackages = with pkgs; [
    vim
    curl
    git
    ghostty.terminfo
  ];

  # leave this alone
  system.stateVersion = lib.mkDefault "24.05";
}
