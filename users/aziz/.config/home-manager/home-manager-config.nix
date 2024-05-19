{self,inputs, pkgs, lib, config, ...}:
let
  #inherit (self) inputs;
 # emacs-overlay = inputs.emacs-overylay.overlay;
in {
 
  home.stateVersion = "23.11";


  home.packages = with pkgs; [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   gcc --version && echo "${config.home.username}"
    # '')
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
    elixir
    erlang
    ocaml
    elixir-ls
    texlive.combined.scheme-full
    cmake
    nixd
    nodePackages_latest.nodejs
    nodePackages_latest.pyright
    #nixfmt-rfc-style
    #  diffoscope
    silver-searcher
    coreutils-full
    gawk
    findutils
    ripgrep-all
    ripgrep
  ];
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
  # programs.emacs = {
  #   enable = true;
  #   package = emacs-with-packages;
  #   extraConfig = ''
  #     (setq default-directory ".config/")
  #   '';
  # };


  programs.emacs = {
      enable = true;
      package = emacs-overlay.emacsPgtk;
      extraConfig = '' '';
      extraPackages = epkgs: (
        with epkgs; [
          magit
          treesit-grammars.with-all-grammars
	  hyperbole
          ace-window
	  avy
          vterm
        ]) ++ (
          with epkgs.melpaPackages; [
            nix-ts-mode
            elixir-ts-mode
            projectile
            helm
            vterm
            languagetool
            s
            ag
	    pdf-tools
	   # info-lookmore
          ]) ++ (with pkgs; []);
    };

  services.emacs = {
    enable = false;
  };
  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. If you don't want to manage your shell through Home
  # Manager then you have to manually source 'hm-session-vars.sh' located at
  # either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/aziz/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = { EDITOR = "emcas"; LC_ALL="en_US.UTF-8"; LANG="en_US.UTF-8"; };

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs.bash.enable = false;
  programs.zsh.enable = false;
  programs.starship.enable = false;
  programs.zellij = {
    enable = false;
    enableFishIntegration = true;
  };
  programs.git = {
    enable = true;
    userEmail = "abdalaziz.rashid@outlook.com";
    userName = "abdalazizrashid";
    delta.enable = true;

    extraConfig.alias = {
      st = "status --short --untracked-files=no";
      fuckme = "reset --hard HEAD";
      fuckyou = "push --force";
      please = "push --force-with-lease";
    };
  };

  imports = [
    # For home-manager
    # nixvim.homeManagerModules.nixvim
    # For NixOS
    #nixvim.nixosModules.nixvi
    # For nix-darwin
    #nixvim.nixDarwinModules.nixvim
    #    "${home.homeDirectory}/.config/tmux.nix"
    
    
  ];
  # Neovim settings
  # programs.nixvim = import ./nvim.nix { inherit pkgs; };
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
        extraConfig = let
          resurrectPrograms' =
            [ "vi" "~vim->vim" "~nvim->nvim" "less" "more" "man" ];
          sep = " ";
          hasWhiteSpaces = p:
            (builtins.match ''
              .*[ 
              
	].*'' p) != null;
          escapeProg = p: if (hasWhiteSpaces p) then ''"${p}"'' else p;
          resurrectPrograms =
            lib.concatMapStringsSep sep escapeProg resurrectPrograms';
        in ''
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
  home.activation.fix-lsr =
    let
      lsr = "/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister";
      xargs = "${pkgs.findutils}/bin/xargs";
      awk = "${pkgs.gawk}/bin/awk";
      rg = "${pkgs.ripgrep}/bin/rg";
      
    in
      ''
      ${lsr} -dump | ${rg} /nix/store | ${awk} '{ print $2 }' | ${xargs} ${lsr} -f -u
      ${lsr} -r $HOME/.local/state/nix/profiles/home-manager/home-path/Applications/
      '';

  programs.fish = {
      enable = true;
      shellAliases = {
	lsr = "/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister";
      };
      shellInit = ''
      '';
    };
  home.file = {
    ".emacs.d/init.el".source =
        config.lib.file.mkOutOfStoreSymlink
          "/Users/aziz/tree-3/users/aziz/.config/emacs/init.el";
    
    ".emacs.d/early-init.el".source =
        config.lib.file.mkOutOfStoreSymlink
          "/Users/aziz/tree-3/users/aziz/.config/emacs/early-init.el";
    };
};

