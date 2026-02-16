# Formatter configuration — perSystem aspect
#
# Configures nixpkgs-fmt as the formatter for all systems.
# This is a flake-parts perSystem configuration.
{ ... }:
{
  systems = [
    "aarch64-darwin"
    "aarch64-linux"
    "x86_64-linux"
  ];
  # Configure the formatter for each system
  perSystem = { pkgs, ... }: {
    formatter = pkgs.nixpkgs-fmt;
  };
}
