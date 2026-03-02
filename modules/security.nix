{ ... }:
{
  flake.modules.nixos.security = {
    security.sudo = {
      enable = true;
      keepTerminfo = true;
      execWheelOnly = true;
      wheelNeedsPassword = false;
    };

    hardware.enableRedistributableFirmware = true;

    boot.kernel.sysctl."kernel.sysrq" = 0;
  };
}
