{ ... }:
{
  flake.modules.nixos.gaming = { config, lib, pkgs, ... }: {
    options.dots.gaming.enable = lib.mkEnableOption "Gaming support (Steam, gamescope, etc.)";

    config = lib.mkIf config.dots.gaming.enable {
      assertions = [
        {
          assertion = config.dots.graphical.available;
          message = "dots.gaming.enable requires a graphical session";
        }
      ];

      programs.steam = {
        enable = true;
        gamescopeSession.enable = true;
        remotePlay.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
      };

      programs.gamescope = {
        enable = true;
        capSysNice = true;
      };

      environment.systemPackages = with pkgs; [
        mangohud
        prismlauncher
        steam-devices-udev-rules
      ];

      boot.kernel.sysctl = {
        "kernel.sched_cfs_bandwidth_slice_us" = 3000;
        "net.ipv4.tcp_fin_timeout" = 5;
        "kernel.split_lock_mitigate" = 0;
        "vm.max_map_count" = 2147483642;
      };
    };
  };
}
