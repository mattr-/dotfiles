{ ... }:
{
  flake.modules.nixos.graphics = { config, lib, pkgs, ... }: lib.mkIf config.dots.graphics.enable {
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
        ];
      extraPackages32 = with pkgs.pkgsi686Linux; [
        libva-vdpau-driver
        libvdpau-va-gl
      ];
    };

    services.xserver.videoDrivers = lib.mkDefault (
      if config.hardware.gpu == "intel" then [ "intel" ]
      else if config.hardware.gpu == "nvidia" then [ "nvidia" ]
      else if config.hardware.gpu == "amd" then [ "amdgpu" ]
      else [ ]
    );

    boot.kernelParams = lib.mkIf (config.hardware.gpu == "amd") [
      "amd_iommu=off"
      "amdgpu.lockup_timeout=5000,10000,10000,5000"
      "ttm.pages_min=2097152"
      "amdgpu.sched_hw_submission=4"
    ];

    hardware.nvidia = lib.mkIf (config.hardware.gpu == "nvidia") {
      modesetting.enable = lib.mkDefault true;
      powerManagement.enable = lib.mkDefault true;
      open = lib.mkDefault false;
      nvidiaSettings = lib.mkDefault true;
    };
  };
}
