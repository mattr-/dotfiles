{ lib, ... }:
{
  flake.modules.nixos.stateVersion = {
    system.stateVersion = lib.mkDefault "24.05";
  };

  flake.modules.darwin.stateVersion = {
    system.stateVersion = lib.mkDefault 7;
    system.configurationRevision = outputs.self.rev or outputs.self.dirtyRev or null;
  };

  flake.modules.homeManager.stateVersion = {
    home.stateVersion = lib.mkDefault "24.05";
  };
}
