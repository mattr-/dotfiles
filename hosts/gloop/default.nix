{inputs, outputs, ...}: {
  imports = [
    # Add my defaults for Darwin
    ../_global/darwin

    # Set up my user
    ../../users/mattr-/darwin.nix
  ];

  networking = {
    hostName = "gloop";
  };

  system = {
    # Set Git commit hash for darwin-version
    configurationRevision = outputs.self.rev or outputs.self.dirtyRev or null;

    # Used for backwards compat. Read the changelog
    # $ darwin-rebuild changelog
    stateVersion = 5;
  };

  nixpkgs.hostPlatform = "aarch64-darwin";
}
