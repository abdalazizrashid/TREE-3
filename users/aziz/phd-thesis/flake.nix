{
  description = "My PhD thesis";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ (import ./overlay.nix) ]; };
        in
          {
            packages = {
	      thesis = pkgs.thesis;
	    };
            defaultPackage = pkgs.thesis;
            devShell = pkgs.mkShell {
              buildInputs = [ pkgs.texliveFull pkgs.texlab pkgs.ghostscript pkgs.tectonic ];
              FONTCONFIG_FILE = pkgs.myFontsConf;
            };
          }
    );
}
