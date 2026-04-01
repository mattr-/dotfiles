{ ... }:
{
  flake.modules.nixos.graphics = { lib, config, pkgs, ... }:
    let
      gpu = config.hardware.gpu;
    in
    {
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
          ++ lib.optionals (gpu == "intel") [
            intel-media-driver
            intel-compute-runtime
          ]
          ++ lib.optionals (gpu == "amd") [
            rocmPackages.clr.icd
          ];
        extraPackages32 = with pkgs.pkgsi686Linux; [
          libva-vdpau-driver
          libvdpau-va-gl
        ];
      };

      services.xserver.videoDrivers = lib.mkDefault (
        if gpu == "intel" then [ "intel" ]
        else if gpu == "nvidia" then [ "nvidia" ]
        else if gpu == "amd" then [ "amdgpu" ]
        else [ ]
      );

      boot.kernelParams = lib.mkIf (gpu == "amd") [
        "amd_iommu=off"
        "amdgpu.lockup_timeout=5000,10000,10000,5000"
        "ttm.pages_min=2097152"
        "amdgpu.sched_hw_submission=4"
      ];

      hardware.nvidia = lib.mkIf (gpu == "nvidia") {
        modesetting.enable = lib.mkDefault true;
        powerManagement.enable = lib.mkDefault true;
        open = lib.mkDefault false;
        nvidiaSettings = lib.mkDefault true;
      };
    };
}
