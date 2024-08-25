{config, lib, pkgs, ...}:
{
imports = let
  # replace this with an actual commit id or tag
  commit = "298b235f664f925b433614dc33380f0662adfc3f";
in [
  "${builtins.fetchTarball {
    url = "https://github.com/Mic92/sops-nix/archive/eb34eb588132d653e4c4925d862f1e5a227cc2ab.tar.gz";
    # replace this with an actual hash
    sha256 = "0000000000000000000000000000000000000000000000000000";
  }}/modules/sops"
];
}
