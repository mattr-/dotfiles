{ ... }:
{
  flake.modules.homeManager.gpg = { pkgs, ... }: {
    programs.gpg = {
      enable = true;
      mutableKeys = true;
      mutableTrust = true;
    };

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      enableExtraSocket = true;
      pinentry.package = pkgs.pinentry-tty;
    };
  };
}
