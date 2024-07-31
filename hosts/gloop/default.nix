{inputs, ...}: {
  imports = [
    # Attempt to configure nix
    ../_global/linux/nix.nix

    # Set up my user
    ../../users/mattr-/darwin.nix
  ];

  networking = {
    hostName = "gloop";
  };
}
