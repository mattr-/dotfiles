{ ... }:
{
  flake.modules.nixos.graphics = { lib, config, pkgs, ... }: {
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs;
        [
          libva
          libva-vdpau-driver
          libvdpau-va-gl
          mesa
        ]
        ++ lib.optionals (config.hardware.gpu == "intel") [
          intel-media-driver
          intel-compute-runtime
        ]
        ++ lib.optionals (config.hardware.gpu == "amd") [
          rocmPackages.clr.icd
        ]
        ++ lib.optionals (config.hardware.gpu == "nvidia") [
        ];
      extraPackages32 = with pkgs.pkgsi686Linux; [
        libva-vdpau-driver
        libvdpau-va-gl
      ];
    };
  };
}
