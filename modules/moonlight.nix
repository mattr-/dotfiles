{ ... }:
{
  flake.modules.nixos.moonlight = { config, lib, pkgs, ... }: lib.mkIf config.dots.moonlight.enable {
    environment.systemPackages = with pkgs; [
      moonlight-qt
    ];
  };
}
