{
  description = "Example Darwin system flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-packages = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    inputs@{
      self,
      home-manager,
      darwin,
      emacs-packages,
      nixpkgs,
    }:
    let
      home-state-version = "24.05";
    in
    {

      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#m1
      darwinConfigurations.m1 = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./nodes/macbook-pro-m1.nix
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.aziz = import ./users/aziz/.config/home-manager/home-manager.nix;
          }
        ];
        specialArgs = {
          inherit inputs self;
        };
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations.m1.pkgs;
    };
}
