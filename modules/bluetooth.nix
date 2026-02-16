{ ... }:
{
  flake.modules.nixos.bluetooth = { pkgs, ... }: {
    hardware.bluetooth = {
      enable = true;
      package = pkgs.bluez5-experimental;
      settings = {
        General = {
          RefreshDiscovery = true;
          KernelExperimental = true;
          Experimental = true;
          FastConnectable = true;
          JustWorksRepairing = "always";
          Privacy = "device";
        };
      };
    };
    boot.extraModprobeConfig = "options bluetooth disable_ertm=1";
  };
}
