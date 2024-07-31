{inputs, ...}: {
  imports = [
    ../_global/darwin/nix.nix

    # Set up my user
    ../../users/mattr-/darwin.nix
  ];

  networking = {
    hostName = "gloop";
  };

  system = {
    # Set Git commit hash for darwin-version
    configurationRevision = self.rev or self.dirtyRev or null;

    # Used for backwards compat. Read the changelog
    # $ darwin-rebuild changelog
    stateVersion = 4;
  };

  nixpkgs.hostPlatform = "aarch64-darwin";
}
