# Formatter configuration — perSystem aspect
#
# Configures nixpkgs-fmt as the formatter for all systems.
# This is a flake-parts perSystem configuration.
{ ... }:
{
  # Configure the formatter for each system
  perSystem = { pkgs, ... }: {
    formatter = pkgs.nixpkgs-fmt;
  };
}
