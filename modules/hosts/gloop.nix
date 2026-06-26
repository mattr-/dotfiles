{ inputs, config, ... }:
let
  darwinModules = builtins.attrValues (config.flake.modules.darwin or { });
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
in
{
  flake.darwinConfigurations."gloop" = inputs.nix-darwin.lib.darwinSystem {
    system = "aarch64-darwin";
    specialArgs = { inherit inputs; };
    modules = darwinModules ++ [
      {
        nixpkgs.hostPlatform = "aarch64-darwin";
        networking = { hostName = "gloop"; };

        hm.imports = hmModules;
      };
    ];
  };
}
