{ inputs, config, ... }:
let
  darwinModules = builtins.attrValues (
    builtins.removeAttrs (config.flake.modules.darwin or { }) [ "home-manager-integration" ]
  );
in
{
  flake.darwinConfigurations."gloop" = inputs.nix-darwin.lib.darwinSystem {
    system = "aarch64-darwin";
    specialArgs = { inherit inputs; };
    modules = darwinModules ++ [
      ({
        nixpkgs.hostPlatform = "aarch64-darwin";
        networking = { hostName = "gloop"; };
      })
    ];
  };
}
