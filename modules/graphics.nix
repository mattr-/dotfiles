{ ... }:
{
  flake.modules.nixos.graphics = { lib, config, pkgs, ... }:
    let
      gpu = config.hardware.gpu;
      btop = if gpu == "amd" then pkgs.btop-rocm
        else if gpu == "nvidia" then pkgs.btop-cuda
        else pkgs.btop;
    in
    {
      environment.systemPackages = [ btop ];

      hardware.graphics = {
        enable = true;
        enable32Bit = config.gui.enable;
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
        extraPackages32 = lib.optionals config.gui.enable (
          with pkgs.pkgsi686Linux; [
            libva-vdpau-driver
            libvdpau-va-gl
          ]
        );
      };

      services.xserver.videoDrivers = lib.mkIf config.gui.enable (
        lib.mkDefault (
          if gpu == "intel" then [ "intel" ]
          else if gpu == "nvidia" && config.gui.enable then [ "nvidia" ]
          else if gpu == "amd" then [ "amdgpu" ]
          else [ ]
        )
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
        nvidiaSettings = lib.mkDefault config.gui.enable;
      };
    };
}
