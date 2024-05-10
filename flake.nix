{
  description = "TREE(3) flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    darwin.url = "github:lnl7/nix-darwin/";
    emacs-overlay.url = "github:nix-community/emacs-overlay";  
    home-manager.url = "github:nix-community/home-manager/master";
    flake-parts.url = "github:hercules-ci/flake-parts";
    mission-control.url = "github:Platonic-Systems/mission-control";
    flake-root.url = "github:srid/flake-root";
  };
  outputs =
    inputs@{
    self,
    home-manager,
    darwin,
    emacs-overlay,
    nixpkgs,
    flake-parts,
    ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      debug = true;
      flake = {
	 # Put your original flake attributes here.
      };
      systems = [
	"aarch64-darwin"
	"x86_64-linux"
      ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        # mission-control = {
        #   wrapperName = "run";
        #   scripts = {
        #     build = {
        #       description = "cowsay";
        #       exec = ''

        #       '';
        #       category = "Development";
        #     };
        #     test = {
        #       description = "cowsay";
        #       exec = ''
        #         cowsay Howdy! .
        #       '';
        #       category = "Development";
        #     };
        #   };
        # };
	devShells.default = pkgs.mkShell {
	  nativeBuildInputs = with pkgs; [ cowsay ];
	  inputsFrom = [ config.mission-control.devShell config.flake-root.devShell ];
	};
      };
      flake = {
	darwinConfigurations = import ./nodes inputs;
      };
    };
}

