{inputs, outputs, ...}: {
  imports = [
    # Configure nix for darwin
    ../_global/darwin/nix.nix

    # Configure Homebrew
    ../_global/darwin/homebrew.nix

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
    stateVersion = 4;
  };

  nixpkgs.hostPlatform = "aarch64-darwin";
}
