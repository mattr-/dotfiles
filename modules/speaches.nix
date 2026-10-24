{ ... }:
{
  flake.modules.nixos.speaches = { config, lib, ... }: {
    options.dots.speaches.enable =
      lib.mkEnableOption "Speaches speech-to-text server";

    config = lib.mkIf config.dots.speaches.enable {
      assertions = [
        {
          assertion = config.hardware.gpu == "nvidia";
          message = "Speaches CUDA support requires hardware.gpu = \"nvidia\".";
        }
      ];

      virtualisation.oci-containers.containers.speaches = {
        image = "ghcr.io/speaches-ai/speaches:latest-cuda";
        ports = [ "8000:8000" ];
        extraOptions = [ "--device=nvidia.com/gpu=all" ];
        volumes = [
          "speaches-models:/home/ubuntu/.cache/huggingface/hub"
        ];
      };
    };
  };
}
