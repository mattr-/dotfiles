{ self, lib, inputs, ... }:

let
  # Get all subdirectories in the hosts directory
  hostDirs = builtins.attrNames (builtins.readDir ./.);

  # Filter out non-directories and any special files/folders (like default.nix itself)
  hostNames = builtins.filter (name:
    name != "default.nix" &&
    name != "_global" &&
    builtins.pathExists (./. + "/${name}/default.nix") &&
    builtins.isAttrs (builtins.readDir (./. + "/${name}"))
  ) hostDirs;

  # Create paths so we can import each host's configuration
  hostModules = map (name: ./. + "/${name}/default.nix") hostNames;
in
{
  imports = hostModules;

  # For debugging: show which hosts were discovered
  _module.args._discoveredHosts = hostNames;
}
