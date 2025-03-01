{...}: {
  disko.devices = {
    disk = {
      nvme = {
        type = "disk";
        device = "/dev/nvme0n1";
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
                mountOptions = ["umask=077"];
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
                };
              };
            };
          };
        };
      };
    };
  };
}
