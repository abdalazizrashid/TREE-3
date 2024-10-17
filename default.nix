{
  npins ? import ./npins,
  nixpkgs ? npins.nixpkgs,
  pkgs ? import nixpkgs { },
  nixosSystem ? import (nixpkgs + "/nixos/lib/eval-config.nix"),
}:

{
  nodes.d1 = nixosSystem {
    system = [ "x86_64-linux" ];
    specialArgs = {
      inputs = npins;
    };
    modules = [ ./nodes/devbox/configuration.nix ];
  };
  nodes.m1-linux = nixosSystem {
    system = [ "arch64-linux" ];
    specialArgs = {
      inputs = npins;
    };
    modules = [ ./nodes/m1-linux/configuration.nix ];
  };
  nodes.nl = nixosSystem {
    system = [ "x86_64-linux" ];
    specialArgs = {
      inputs = npins;
    };
    modules = [ ./nodes/nl/configuration.nix ];
  };
  inherit pkgs;
  inherit npins;
}

  
