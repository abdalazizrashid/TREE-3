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
# outputs =
#     inputs@{
#       self,
#       home-manager,
#       darwin,
#       emacs-overlay,
#     nixpkgs,
#     ...
#     }:
#     let
#       home-state-version = "24.05";
#     in
#       {
#       # Build darwin flake using:
#       # $ darwin-rebuild build --flake .#m1
#       darwinConfigurations.m1 = darwin.lib.darwinSystem {
#         system = "aarch64-darwin";
#         modules = [
#           ./nodes/macbook-pro-m1.nix
#           home-manager.darwinModules.home-manager
#           {
#             home-manager.useGlobalPkgs = true;
#             home-manager.useUserPackages = true;
#             home-manager.users.aziz = import ./users/aziz/.config/home-manager/home-manager.nix;
#           }
#         ];
#         specialArgs = {
#           inherit inputs self;
#         };
#       };
#       # Expose the package set, including overlays, for convenience.
#       darwinPackages = self.darwinConfigurations.m1.pkgs;
#       };

  
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
      imports = [
          inputs.mission-control.flakeModule
          inputs.flake-root.flakeModule
	];
      flake = {
	 # Put your original flake attributes here.
      };
      systems = [
	"aarch64-darwin"
	"x86_64-linux"
      ];
      perSystem = { config, self', inputs', pkgs, system, ... }: {
	devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [ cowsay ];
          inputsFrom = [ config.mission-control.devShell config.flake-root.devShell ];
        };
        mission-control = {
          wrapperName = "run";
          scripts = {
            build = {
              description = "cowsay";
              exec = ''

              '';
              category = "Development";
            };
            test = {
              description = "cowsay";
              exec = ''
                cowsay Howdy! .
              '';
              category = "Development";
            };
          };
        };
      };
      flake = {
	darwinConfigurations = import ./nodes inputs;
      };
    };
}

