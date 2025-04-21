let
  server = [
    ./core/boot.nix
    ./core

    ./network
    ./network/avahi.nix

    ./services/ssh.nix
  ];

  desktop =
    server
    ++ [
      ./hardware/fwupd.nix
      ./services/sound.nix
    ];

  laptop =
    desktop
    ++ [
      ./hardware/bluetooth.nix
    ];
in {
  inherit server desktop laptop;
}
