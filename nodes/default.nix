{self, nixpkgs, darwin, home-manager, ...}:
let
  inherit (self) inputs;
  in
  {
    pkgs = nixpkgs.extend (self: super: {
      overlays = [ (import self.inputs.emacs-overlay) ];
    });
    m1 = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        ./macbook-pro-m1.nix
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.aziz = import ../users/aziz/.config/home-manager/home-manager.nix;
        }
      ];
      specialArgs = {
        inherit inputs self;
      };
    };
    specialArgs = {inherit inputs;};
    darwinPackages = self.darwinConfigurations.m1.pkgs;

  }
