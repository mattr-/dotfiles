{ inputs, ... }:
{
  flake.modules.homeManager.ai = { pkgs, config, lib, ... }: {
    config = lib.mkIf config.ai.enable {
      home.packages = with inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
        claude-code
        copilot-cli
        omp
        opencode
        pi
      ];
    };
  };
}
