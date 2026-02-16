{ lib, ... }:
{
  flake.modules.nixos.locale = {
    i18n.defaultLocale = lib.mkDefault "en_US.UTF-8";
  };
}
