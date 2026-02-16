{ ... }:
{
  flake.modules.nixos.graphics = { lib, config, pkgs, ... }: {
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs;
        [ ]
        ++ lib.optionals (config.hardware.gpu == "intel") [
          intel-media-driver
          intel-compute-runtime
        ]
        ++ lib.optionals (config.hardware.gpu == "amd") [
          rocmPackages.clr.icd
        ]
        ++ lib.optionals (config.hardware.gpu == "nvidia") [
        ];
    };
  };
}
