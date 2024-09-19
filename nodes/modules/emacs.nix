{lib, config, pkgs, ...}:
let

in  
{
  nixpkgs.overlays = [
    (import (
      builtins.fetchTarball {
        url = "https://github.com/nix-community/emacs-overlay/archive/fa117fc7f6ad4f7466e305608c72688947da5b5d.zip";
      }
    ))
  ];
  
  services.emacs = {
      enable = true;
      package = pkgs.emacs-pgtk;
    };

}

  
