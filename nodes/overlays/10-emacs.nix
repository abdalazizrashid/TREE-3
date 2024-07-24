final: prev:
let
  emacsOverlay = import (
    builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }
  );
  pkgs = import final.path { overlays = [ emacsOverlay ]; };
  # https://github.com/jwiegley/nix-config/blob/5bcdf90eb70c9a271958bd4ad973b2afd43dbdb5/overlays/10-emacs.nix#L705
  emacs29MacPortAlt = pkgs.emacs29-macport.overrideAttrs (o: {
    buildInputs =
      o.buildInputs
      ++ (with pkgs.darwin.apple_sdk_11_0.frameworks; [
        UniformTypeIdentifiers
        Accelerate
      ]);
    patches = o.patches ++ [ ./emacs/0002-mac-gui-loop-block-autorelease.patch ];
  });

  emacs =
    if final.stdenv.targetPlatform.isx86_64 then
      pkgs.emacs29-macport
    else
      (emacs29MacPortAlt).overrideAttrs (o: {
        configureFlags = o.configureFlags ++ [ "--with-natural-title-bar" ];
      });
  emacsEnv = pkgs.emacsWithPackages (epkgs: with epkgs; [ ]);

in
{

  emacs29Env =
    myPkgs:
    prev.myEnvFun {
      name = "emacs29";
      buildInputs = [ (pkgs.emacsWithPackages myPkgs) ];
    };
  emacs29MacPortFromUsePackageEnv = pkgs.emacsWithPackagesFromUsePackage {
    config = ../../users/aziz/.config/emacs/init.el;
    package = emacs;

  };
  emacs29MacPortEnv = pkgs.emacs29-macport;
}
