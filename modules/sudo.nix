{ ... }:
{
  flake.modules.nixos.sudo = {
    security.sudo = {
      enable = true;
      execWheelOnly = true;
      keepTerminfo = true;
      wheelNeedsPassword = false;
    };
  };
}
