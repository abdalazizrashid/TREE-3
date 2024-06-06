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

{ config, pkgs, ... }:
let
  home-manager = import <home-manager> { };
in
{
  imports = [ ./nodes/default.nix ];
}
