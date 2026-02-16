{ ... }:
let
  vmMachineConfig = {
    memorySize = 8192;
    cores = 4;
  };
in
{
  flake.modules.nixos.vm = {
    virtualisation.vmVariant.virtualisation = vmMachineConfig;
    virtualisation.vmVariantWithBootLoader.virtualisation = vmMachineConfig;
  };
}
