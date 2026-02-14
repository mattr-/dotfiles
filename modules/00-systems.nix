# Formatter configuration — flake-parts aspect
#
# Configures nixpkgs-fmt as the formatter for all systems.
{ ... }:
{
  # Configure the formatter for each system
  perSystem = { pkgs, ... }: {
    formatter = pkgs.nixpkgs-fmt;
  };
}
