# { nixpkgs ? <nixpkgs>
# , pkgs ? import nixpkgs { inherit system; }
# , lib ? pkgs.lib
# , system ? builtins.currentSystem

# }:
# let
#   darwin = import <darwin> {};
#   darwin-config = "";
#   home-manager = import <home-manager> {};
#   node = import ./nodes {inherit pkgs darwin home-manager; };

#   in
#   node.m1

{ sources ? import nix/sources.nix, ... }:
let
  pkgs = import sources.nixpkgs { overlays = []; config = {}; };
  home-manager = import sources.home-manager { };
in
{
  imports = [ ./nodes/default.nix ];
}
