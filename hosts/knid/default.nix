{inputs, outputs, ...}: {
  imports = [
    # Set up Darwin defaults
    ../_global/darwin

    # Set up my user
    ../../users/mattr-/darwin.nix
  ];

  networking = {
    hostName = "knid";
  };

  system = {
    # Set Git commit hash for darwin-version
    configurationRevision = outputs.self.rev or outputs.self.dirtyRev or null;

    # Used for backwards compat. Read the changelog
    # $ darwin-rebuild changelog
    stateVersion = 4;
  };

  nixpkgs.hostPlatform = "aarch64-darwin";
}
