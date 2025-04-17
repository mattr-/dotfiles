# Custom packages, that can be defined similarly to ones from nixpkgs
# You can build them using 'nix build .#example'
pkgs: {
  # example = pkgs.callPackage ./example { };
  SF-Pro = pkgs.callPackage ./SF-Pro {inherit (pkgs) stdenv;};
  SF-Pro-mono = pkgs.callPackage ./SF-Pro-mono {};
}
