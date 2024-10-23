{
  pkgs,
  lib,
  config,
  ...
}:
let
in
# launcher = pkgs.writeScript "emacsclient" ''
#   on open location this_URL
#     do shell script "${emacs}/bin/emacsclient -n -a ${emacs}/Applications/Emacs.app/Contents/MacOS/Emacs '" & this_URL & "'"
#     tell application "Emacs" to activate
#   end open location
# '';
{

  home.stateVersion = "24.05";
  home.packages = (
    with pkgs;
    [
      ###  awscli
      python3
      ripgrep
      sd
      fd
      tree
      fzf
      docker
      curl
      bat
      man
      tmux
      tree-sitter
      screen
      gcc
 #     elixir
#      erlang
      ocaml
 #     elixir-ls
      texlive.combined.scheme-full
      cmake
      nixd
      nodePackages_latest.nodejs
##      nodePackages_latest.pyright
      nixfmt-rfc-style
      #  diffoscope
      silver-searcher
      coreutils-full
      gawk
      findutils
      ripgrep-all
      ripgrep
      atuin # Replacement for a shell history
      gh
      fd
      vivid # Generator for LS_COLORS with support for multiple color themes
      git-lfs
      eaglemode
      signal-desktop
      libreoffice
      quassel
    ]
  );
  # programs.emacs = {
  #   enable = true;
  #   package = pkgs.emacs29MacPortFromUsePackageEnv;
  # };

  home.sessionVariables = {
    EDITOR = "emcas";
    LC_ALL = "en_US.UTF-8";
    LANG = "en_US.UTF-8";
    LS_COLORS = "$(vivid generate molokai)";
  };

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs.zsh.enable = false;
  programs.starship.enable = false;
  programs.zellij = {
    enable = false;
    enableFishIntegration = true;
  };
  programs.git = {
    enable = true;
    userEmail = "ping@aziz.fyi";
    userName = "Aziz";
    delta.enable = true;

    extraConfig.alias = {
      st = "status --short --untracked-files=no";
      fuckme = "reset --hard HEAD";
      fuckyou = "push --force";
      please = "push --force-with-lease";
    };
  };
  # Neovim settings
  #programs.nixvim = import ./nvim.nix { inherit pkgs; };
  programs.tmux = {
    enable = false;
    keyMode = "emacs";

    plugins = with pkgs.tmuxPlugins; [
      sensible
      gruvbox
      cpu
      pain-control
      fpp
      {
        plugin = resurrect;
        extraConfig =
          let
            resurrectPrograms' = [
              "vi"
              "~vim->vim"
              "~nvim->nvim"
              "less"
              "more"
              "man"
            ];
            sep = " ";
            hasWhiteSpaces =
              p:
              (builtins.match ''
                              .*[ 
                              
                	].*'' p) != null;
            escapeProg = p: if (hasWhiteSpaces p) then ''"${p}"'' else p;
            resurrectPrograms = lib.concatMapStringsSep sep escapeProg resurrectPrograms';
          in
          ''
            set -g @ressurect-processes '${resurrectPrograms}'
            set -g @resurrect-strategy-vim 'session'
          '';
      }
      {
        plugin = continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval 'TODO'
        '';
      }
    ];
  };
  # targets.darwin = {
  #   keybindings = {
  #     "~f"    = "moveWordForward:";
  #     "~b"    = "moveWordBackward:";

  #     "~d"    = "deleteWordForward:";
  #     "~^h"   = "deleteWordBackward:";
  #     # "~\010" = "deleteWordBackward:";
  #     # "~\177" = "deleteWordBackward:";

  #     "~v"    = "pageUp:";
  #     "^v"    = "pageDown:";

  #     "~&lt;" = "moveToBeginningOfDocument:";
  #     "~&gt;" = "moveToEndOfDocument:";

  #     "^/"    = "undo:";
  #     "~/"    = "complete:";

  #     "^g"    = "_cancelKey:";
  #     "^a"    = "moveToBeginningOfLine:";
  #     "^e"    = "moveToEndOfLine:";

  #     "~c"	  = "capitalizeWord:"; /* M-c */
  #     "~u"	  = "uppercaseWord:";	 /* M-u */
  #     "~l"	  = "lowercaseWord:";	 /* M-l */
  #     "^t"	  = "transpose:";      /* C-t */
  #     "~t"	  = "transposeWords:"; /* M-t */
  #   };
  # };

  # home.activation.fixLsr =
  #   let
  #     lsr = "/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister";
  #     xargs = "${pkgs.findutils}/bin/xargs";
  #     awk = "${pkgs.gawk}/bin/awk";
  #     rg = "${pkgs.ripgrep}/bin/rg";
  #   in
  #   ''
  #     ${lsr} -dump | ${rg} /nix/store | ${awk} '{ print $2 }' | xargs ${lsr} -f -u
  #     ${lsr} -r $HOME/.local/state/nix/profiles/home-manager/home-path/Applications/
  #   '';

  # # home.activation.createEmacsClientApp = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  #   $VERBOSE_ECHO 'Creating EmacsClient.app'
  #   pushd '${config.home.homeDirectory}/Applications'
  #   $DRY_RUN_CMD rm -rf $VERBOSE_ARG EmacsClient.app || true
  #   $DRY_RUN_CMD /usr/bin/osacompile -o EmacsClient.app ${launcher}
  #   $DRY_RUN_CMD /usr/bin/plutil -insert CFBundleURLTypes -json ${lib.escapeShellArg infoPlist} EmacsClient.app/Contents/Info.plist
  #   popd
  # '';

  # home.activation = {
  #   copyApplications = let
  #     apps = pkgs.buildEnv {
  #       name = "home-manager-applications";
  #       paths = config.home.packages;
  #       pathsToLink = "/Applications";
  #     };
  #   in lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  #     baseDir="$HOME/Applications/Home Manager Apps"
  #     if [ -d "$baseDir" ]; then
  #       rm -rf "$baseDir"
  #     fi
  #     mkdir -p "$baseDir"
  #     for appFile in ${apps}/Applications/*; do
  #       target="$baseDir/$(basename "$appFile")"
  #       $DRY_RUN_CMD cp ''${VERBOSE_ARG:+-v} -fHRL "$appFile" "$baseDir"
  #       $DRY_RUN_CMD chmod ''${VERBOSE_ARG:+-v} -R +w "$target"
  #     done
  #   '';
  # };
  programs.fish = {
    enable = false;
    # shellAliases = {
    #   lsr =
    #     "/System/Library/Frameworks/CoreServices.framework/"
    #     + "Frameworks/LaunchServices.framework/Support/lsregister";
    # };
    shellInit = ''
      atuin init fish | source
    '';
  };
  programs.bash = {
    enable = true;
    enableCompletion = true;
#    interactiveShellInit = ''
 #     atuin init bash | source
  #  '';
  };

 # home.file = {
    #    ".emacs.d/init.el".source = config.lib.file.mkOutOfStoreSymlink "/Users/aziz/tree-3/users/aziz/.config/emacs/init.el";
    #   ".emacs.d/early-init.el".source = config.lib.file.mkOutOfStoreSymlink "/Users/aziz/tree-3/users/aziz/.config/emacs/early-init.el";
    #    ".emacs.d/capture-frame.el".source = config.lib.file.mkOutOfStoreSymlink "/Users/aziz/tree-3/users/aziz/.config/emacs/capture-frame.el";
    #    ".emacs.d/snippets/".source = config.lib.file.mkOutOfStoreSymlink "/Users/aziz/tree-3/users/aziz/snippets";
#    ".emacs.d/".source = config.lib.file.mkOutOfStoreSymlink "/home/afdee1c/Sources/tree-3/users/aziz/.config/emacs";
  #};
}
