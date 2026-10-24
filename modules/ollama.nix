{ ... }:
{
  flake.modules.nixos.ollama = { config, lib, ... }:
    let
      gpu = config.hardware.gpu;
      package = if gpu == "amd" then "ollama-rocm"
            else if gpu == "nvidia" then "ollama-cuda"
            else if gpu == "intel" then "ollama-vulkan"
            else "ollama-cpu";
    in
    {
      options.dots.ollama = {
        enable = lib.mkEnableOption "Ollama LLM server with GPU acceleration";
      };

      config = lib.mkIf config.dots.ollama.enable {
        services.ollama = {
          inherit (package);
          enable = true;
          host = "0.0.0.0";
          port = 11434;
          openFirewall = true;
        };
      };
    };
}
