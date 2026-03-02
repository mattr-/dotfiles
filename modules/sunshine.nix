{ ... }:
{
  flake.modules.nixos.sunshine = { config, lib, ... }: {
    options.dots.sunshine.enable = lib.mkEnableOption "Sunshine game streaming server";

    config = lib.mkIf config.dots.sunshine.enable {
      assertions = [
        {
          assertion = config.dots.graphical.available;
          message = "dots.sunshine.enable requires a graphical session";
        }
      ];

      services.sunshine = {
        enable = true;
        autoStart = true;
        capSysAdmin = true;
        openFirewall = true;
      };
    };
  };
}
