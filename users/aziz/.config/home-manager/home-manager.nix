{ config, pkgs, fetchpatch, lib, ... }:

let
  fromGitHub = ref: repo:
    pkgs.vimUtils.buildVimPlugin {
      pname = "${lib.strings.sanitizeDerivationName repo}";
      version = ref;
      src = builtins.fetchGit {
        url = "https://github.com/${repo}.git";
        ref = ref;
      };
    };
    
  nixvim = import (builtins.fetchGit {
    url = "https://github.com/nix-community/nixvim";
    # If you are not running an unstable channel of nixpkgs, select the corresponding branch of nixvim.
    # ref = "nixos-unstable";
  });
  
  # emacs = pkgs.emacs29-macport.overrideAttrs (old: {
  #       patches =
  #         (old.patches or [])
  #         ++ [
  #           # Fix OS window role (needed for window managers like yabai)
  #           (fetchpatch {
  #             url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
  #             sha256 = "0c41rgpi19vr9ai740g09lka3nkjk48ppqyqdnncjrkfgvm2710z";
  #           })
  #           # Use poll instead of select to get file descriptors
  #           (fetchpatch {
  #             url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/poll.patch";
  #             sha256 = "0j26n6yma4n5wh4klikza6bjnzrmz6zihgcsdx36pn3vbfnaqbh5";
  #           })
  #           # Enable rounded window with no decoration
  #           (fetchpatch {
  #             url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/round-undecorated-frame.patch";
  #             sha256 = "111i0r3ahs0f52z15aaa3chlq7ardqnzpwp8r57kfsmnmg6c2nhf";
  #           })
  #           # Make Emacs aware of OS-level light/dark mode
  #           (fetchpatch {
  #             url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
  #             sha256 = "14ndp2fqqc95s70fwhpxq58y8qqj4gzvvffp77snm2xk76c1bvnn";
  #           })
  #         ];
  #     });
  #   emacs-overlay = import (fetchTarball {
#       url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
#       sha256 = "1jppksrfvbk5ypiqdz4cddxdl8z6zyzdb2srq8fcffr327ld5jj2";
#     });
#     my-emacs = pkgs.emacs.override {
#        withNativeCompilation = true;
#        withSQLite3 = true;
#        withTreeSitter = true;
#        withWebP = true;
#        withNS = true;
#     };

#      emacs-with-packages = (pkgs.emacsPackagesFor my-emacs).emacsWithPackages (epkgs: with epkgs; [
#        hyperbole
#        ace-window
#        vterm
#        pdf-tools
#        treesit-grammars.with-all-grammars
#        magit
#        helm
#        projectile
#        nix-ts-mode
#        elixir-ts-mode
#        projectile
#        s

#   #  emacs-all-the-icons-fonts
#   #  (aspellWithDicts (d: [d.en d.sv]))
# #    ghostscript
#  #   tetex
#  #   poppler
#  #   mu
#  #   wordnet
# ]);

in {
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = "https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz";
    }))
  ];
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
    nixfmt-rfc-style
    #  diffoscope
    silver-searcher
    coreutils-full
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
      package = pkgs.emacs29-macport;
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
    nixvim.homeManagerModules.nixvim
    # For NixOS
    #nixvim.nixosModules.nixvim
    # For nix-darwin
    #nixvim.nixDarwinModules.nixvim
    #    "${home.homeDirectory}/.config/tmux.nix"
  ];
  # Neovim settings
  programs.nixvim = import ./nvim.nix { inherit pkgs; };
  programs.tmux = {
    enable = true;
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

  # 30: what if this is defined in another file? Merge it!
  programs.fish = {
      enable = true;
      # shellAliases = {
      #     emacs = "${pkgs.emacs}/Applications/Emacs.app/Contents/MacOS/Emacs";
      # };
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
}
