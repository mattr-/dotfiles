{...}: {
  disko.devices = {
    disk = {
      nvme = {
        type = "disk";
        device = "/dev/disk/by-id/nvme-eui.e8238fa6bf530001001b448b417d330c";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              name = "ESP";
              size = "512M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            root = {
              size = "100%";
              content = {
                type = "btrfs";
                extraArgs = ["-f"]; # Override existing partitions
                subvolumes = {
                  "/root" = {
                    mountOptions = ["compress=zstd"];
                    mountpoint = "/";
                  };
                  "/home" = {
                    mountOptions = ["compress=zstd"];
                    mountpoint = "/home";
                  };
                  "/nix" = {
                    mountOptions = ["compress=zstd" "noatime"];
                    mountpoint = "/nix";
                  };
                  "/mc-po3mythic" = {
                    mountOptions = ["noatime"];
                    mountpoint = "/var/lib/mc-po3mythic";
                  };
                  "/mc-atm9sky" = {
                    mountOptions = ["noatime"];
                    mountpoint = "/var/lib/mc-atm9sky";
                  };
                  "/mc-atm9" = {
                    mountOptions = ["noatime"];
                    mountpoint = "/var/lib/mc-atm9";
                  };
                  "/mc-stoneblock" = {
                    mountOptions = ["noatime"];
                    mountpoint = "/var/lib/mc-stoneblock";
                  };
                  "/mc-stoneblock2" = {
                    mountOptions = ["noatime"];
                    mountpoint = "/var/lib/mc-stoneblock2";
                  };
                  "/mc-stoneblock3" = {
                    mountOptions = ["noatime"];
                    mountpoint = "/var/lib/mc-stoneblock3";
                  };
                  "/mc-atm10" = {
                    mountOptions = ["noatime"];
                    mountpoint = "/var/lib/mc-atm10";
                  };
                };
              };
            };
          };
        };
      };
    };
  };
}
