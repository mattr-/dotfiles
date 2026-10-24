{ ... }:
{
  flake.modules.nixos.docker = { config, ... }: {
    virtualisation = {
      oci-containers.backend = "podman";

      podman = {
        enable = true;
        dockerCompat = true;
      };
    };

    hardware.nvidia-container-toolkit.enable = config.hardware.gpu == "nvidia";


  };
}
