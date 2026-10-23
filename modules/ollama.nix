{ ... }:
{
  flake.modules.nixos.ollama = { config, lib, ... }:
    let
      gpu = config.hardware.gpu;
      acceleration = if gpu == "amd" then "rocm"
            else if gpu == "nvidia" then "cuda"
            else null;
    in
    {
      options.dots.ollama = {
        enable = lib.mkEnableOption "Ollama LLM server with GPU acceleration";
      };

      config = lib.mkIf config.dots.ollama.enable {
        services.ollama = {
          enable = true;
          host = "0.0.0.0";
          port = 11434;
          openFirewall = true;
          acceleration = if config.hardware.gpu == "amd" then "rocm"
            else if config.hardware.gpu == "nvidia" then "cuda"
            else null;
        };
      };
    };
}
