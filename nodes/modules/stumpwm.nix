{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.T.stumpwm;
  stumpwm = pkgs.sbclPackages.stumpwm.overrideAttrs (old: rec {
   version = "24.11";
   src = pkgs.fetchFromGitHub {
      owner = "stumpwm";
      repo = "stumpwm";
      rev = version;
      hash = "sha256-zXj17ucgyFhv7P0qEr4cYSVRPGrL1KEIofXWN2trr/M=";
     };     
   }
  );
in
{
  options = {
    T.stumpwm.enable = lib.mkEnableOption "Enable stumpwm on this node";
  };
  config = lib.mkIf cfg.enable {
  services.xserver.enable = true;

  services.xserver.displayManager.startx.enable = true;
  
  #services.xserver.windowManager.stumpwm.enable = true;

# bigger tty fonts
  console.font =
    "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";
  services.xserver.dpi = 180;
  environment.variables = {
    ## Used by GTK 3
    # `GDK_SCALE` is limited to integer values
    GDK_SCALE = "2";
    # Inverse of GDK_SCALE
    GDK_DPI_SCALE = "0.5";

    # Used by Qt 5
    QT_AUTO_SCREEN_SCALE_FACTOR = "1";

    _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
  };
  # Expose variables to graphical systemd user services
  services.xserver.displayManager.importedVariables = [
    "GDK_SCALE"
    "GDK_DPI_SCALE"
    "QT_AUTO_SCREEN_SCALE_FACTOR"
  ];


  services.xserver.windowManager.session = lib.singleton {
      name = "stumpwm";
      start = ''
        ${pkgs.sbclPackages.stumpwm}/bin/stumpwm &
        waitPID=$!
      '';
    };
    environment.systemPackages = [ stumpwm ];
  };
}
