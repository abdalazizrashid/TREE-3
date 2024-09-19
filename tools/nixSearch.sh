cat packages.txt                                \
  | fzf --prompt="Nixpkgs> " --reverse                      \
        --preview-window=wrap:70%                     \
        --preview="echo -e \"{1}\n\"; nix-env -qa --description -A {1}"
