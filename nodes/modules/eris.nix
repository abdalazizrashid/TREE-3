{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.T.eris;
  eris = pkgs.eris-go.overrideAttrs (old: {
    src = pkgs.fetchFromGitea {
      domain = "codeberg.org";
      owner = "eris";
      repo = "eris-go";
      rev = "20241028";
      hash = "sha256-v4pN+fVwYoir3GLneWhg/azsg7ifvcKAksoqDkkQGwk=";
    };

    vendorHash = "sha256-0BI4U9p4R7umyXtHAQBLa5t5+ni4dDndLNXgTIAMsqw=";
  });
in
{
  options = {
    T.eris.enable = lib.mkEnableOption "Enable eris server on this node";
    #T.eris.storagePath = lib.mkOption { description = "Storage backend for eris"; };
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ eris-go ];
    services.eris-server = {
      package = eris;
      enable = true;
      decode = true;
      
      backends = [
        "dir+file:///tank/eris/?get&put"
      ];
    };
  };
}
