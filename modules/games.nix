{ ... }:
{
  flake.modules.nixos.games = { pkgs, ... }: {
    services.sunshine = {
      enable = true;
      autoStart = true;
      capSysAdmin = true;
      openFirewall = true;
    };

    environment.systemPackages = with pkgs; [
      moonlight-qt
    ];
  };
}

