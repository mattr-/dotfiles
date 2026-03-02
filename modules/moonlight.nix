{ ... }:
{
  flake.modules.nixos.moonlight = { config, lib, pkgs, ... }: {
    options.dots.moonlight.enable = lib.mkEnableOption "Moonlight game streaming client";

    config = lib.mkIf config.dots.moonlight.enable {
      assertions = [
        {
          assertion = config.dots.graphical.available;
          message = "dots.moonlight.enable requires a graphical session";
        }
      ];

      environment.systemPackages = with pkgs; [
        moonlight-qt
      ];
    };
  };
}
