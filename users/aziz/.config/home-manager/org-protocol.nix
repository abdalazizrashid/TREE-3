{
  config,
  lib,
  pkgs,
  ...
}:
let
  infoPlist = builtins.toJSON [
    {
      CFBundleURLName = "org-protocol handler";
      CFBundleURLSchemes = [ "org-protocol" ];
    }
  ];

  emacs = config.programs.emacs.finalPackage;

  launcher = pkgs.writeScript "emacsclient" ''
    on open location this_URL
      do shell script "${emacs}/bin/emacsclient -n -a ${emacs}/Applications/Emacs.app/Contents/MacOS/Emacs '" & this_URL & "'"
      tell application "Emacs" to activate
    end open location
  '';
in
{
  config.home.activation.createEmacsClientApp = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $VERBOSE_ECHO 'Creating EmacsClient.app'
    pushd '${config.home.homeDirectory}/Applications'
    $DRY_RUN_CMD rm -rf $VERBOSE_ARG EmacsClient.app || true
    $DRY_RUN_CMD /usr/bin/osacompile -o EmacsClient.app ${launcher}
    $DRY_RUN_CMD /usr/bin/plutil -insert CFBundleURLTypes -json ${lib.escapeShellArg infoPlist} EmacsClient.app/Contents/Info.plist
    popd
  '';
}
