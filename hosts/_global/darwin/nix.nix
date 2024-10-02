{
  inputs,
  outputs,
  pkgs,
  lib,
  config,
  ...
}: {
  nix = let
    flakeInputs = lib.filterAttrs (_: lib.isType "flake") inputs;
  in {
    settings = {
      auto-optimise-store = lib.mkDefault false;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      warn-dirty = false;

      # Workaround for https://github.com/NixOS/nix/issues/9574
      nix-path = config.nix.nixPath;
    };

    # Use the latest version we can get
    package = pkgs.nixVersions.latest;

    # Automatic garbage collection
    gc = {
      automatic = true;
      # Emulate crontab's `weekly` setting, equivalent to `0 0 * * 0`
      # https://crontab.guru/#0_0_*_*_0
      interval = {
        Minute = 0;
        Hour = 0;
        Weekday = 0;
      };
      options = "--delete-older-than 30d";
    };
  };

  nixpkgs = {
    # Add our overlays here
    overlays = [
      # Add overlays from our own flake exports
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
    ];

    # Allow unfree packages
    config = {
      allowUnfree = true;
    };
  };

  # Have NVD report changes
  # Adapted from https://github.com/JamieMagee/nix-config/blob/0de80fe6efde4f7ff33b2500d18edeadf2de98d4/hosts/common/global/nix.nix
  system.activationScripts.system-report-changes = ''
    PATH=$PATH:${
      lib.makeBinPath [
        pkgs.nvd
        pkgs.nix
      ]
    }
    # Disable nvd is there are less than two profiles in the system.
    if [ $(ls -d1v /nix/var/nix/profiles/system-*-link 2>/dev/null | wc -l) -lt 2 ]
    then
      echo "Skipping system change report..."
    else
      nvd diff $(ls -d1v /nix/var/nix/profiles/system-*-link | tail -n2)
    fi
  '';

  # Enable the nix daemon service
  services.nix-daemon.enable = true;
}
