{ inputs, ... }:
let
  commonHomeManagerOptions = {
    backupFileExtension = "bak";
    extraSpecialArgs = { inherit inputs; };
    useGlobalPkgs = true;
    useUserPackages = true;
  };
in
{
  flake.modules.nixos.home-manager-integration = { lib, ... }: {
    imports = [
      inputs.home-manager.nixosModules.home-manager

      { home-manager = commonHomeManagerOptions; }

      (lib.mkAliasOptionModule [ "hm" ] [ "home-manager" "users" "mattr-" ])
    ];
  };

  flake.modules.darwin.home-manager-integration = { lib, ... }: {
    imports = [
      inputs.home-manager.darwinModules.home-manager

      { home-manager = commonHomeManagerOptions; }

      (lib.mkAliasOptionModule [ "hm" ] [ "home-manager" "users" "mattr-" ])
    ];
  };
}
