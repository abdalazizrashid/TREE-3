{
  config,
  pkgs,
  darwin,
  home-manager,
  ...
}:

let
  username = "aziz";
in
{
  imports = [ <home-manager/nix-darwin> ];

  # Define the system configuration

  # system = "aarch64-darwin";

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = false;
    users.${username} = {
      imports = [ ../users/${username}/.config/home-manager/home-manager.nix ];
    };
  };

  services.nix-daemon.enable = true;
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowInsecure = false;
      allowUnsupportedSystem = false;

      permittedInsecurePackages = [
        "python-2.7.18.7"
        "libressl-3.4.3"
      ];
    };

    overlays =
      let
        path = ../nodes/overlays;
      in
      with builtins;
      map (n: import (path + ("/" + n))) (
        filter (n: match ".*\\.nix" n != null || pathExists (path + ("/" + n + "/default.nix"))) (
          attrNames (readDir path)
        )
      )
      ++ [
        #(import ./envs.nix)
      ];
  };
  # NixOS configuration.
  nix = {
    useDaemon = true;
    settings = {
      substituters = [
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "cache.c1.s1.black.hole-1:ahIxOKW/NU+VPcLQRA0bLdsPVtRuMBkqaz5jawJYbDg=aziz@c1"
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };

    # Optimize storage space by hardlinking identical derivations in /nix/store
    extraOptions = ''
      auto-optimise-store = true
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
    configureBuildUsers = true;
  };
  # Nix is installed externally 
  # services.nix-daemon.enable = true;
  services.dnsmasq = {

    enable = true;

    addresses = {
      localhost = "127.0.0.1";
      "c1.s1.com" = "10.23.4.78";
    };
  };

  programs.zsh.enable = true;
  programs.zsh.shellInit = ''
    if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
      . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
    fi
  '';

  programs.fish.enable = true;
  programs.fish.shellInit = ''
    if test -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
      source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish'
    end
  '';

  environment.darwinConfig = "/Users/${username}/tree-3/nodes/default.nix";
  environment.shells = with pkgs; [
    bashInteractive
    zsh
    fish
  ];
  # Apps
  # `home-manager` currently has issues adding them to `~/Applications`
  # Issue: https://github.com/nix-community/home-manager/issues/1341
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    cachix
    neovim
    coreutils
    gnugrep
    ripgrep-all
    findutils
  ];

  # https://github.com/nix-community/home-manager/issues/423
  environment.variables = {
    # TERMINFO_DIRS = "${pkgs.kitty.terminfo.outPath}/share/terminfo";
  };
  programs.nix-index.enable = true;

  # Fonts
  fonts.fontDir.enable = true;
  fonts.fonts = with pkgs; [
    recursive
    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
  ];

  # Add ability to used TouchID for sudo authentication
  security.pam.enableSudoTouchIdAuth = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
  launchd.user.envVariables = { };
  networking.computerName = "Aziz Macbook";
  # The platform the configuration will be used on.
  users.users.aziz = {
    name = "aziz";
    home = "/Users/aziz";
  };
  networking.knownNetworkServices = [
    "Wi-Fi 2"
    "Ethernet Adaptor"
    "Thunderbolt Ethernet"
  ];

  networking.dns = [
    "127.0.0.1"
    "8.8.8.8"
    "8.8.4.4"
    "2001:4860:4860::8888"
    "2001:4860:4860::8844"
  ];

  system = {
    defaults = {
      NSGlobalDomain = {
        AppleKeyboardUIMode = 3;
        AppleInterfaceStyle = "Dark";
        AppleShowAllExtensions = true;
        NSAutomaticWindowAnimationsEnabled = false;
        NSNavPanelExpandedStateForSaveMode = true;
        NSNavPanelExpandedStateForSaveMode2 = true;
        "com.apple.keyboard.fnState" = false;
        _HIHideMenuBar = true;
        "com.apple.mouse.tapBehavior" = 1;
        "com.apple.sound.beep.volume" = 0.0;
        "com.apple.sound.beep.feedback" = 0;
        ApplePressAndHoldEnabled = false;
      };

      CustomUserPreferences = {
        "com.apple.finder" = {
          ShowExternalHardDrivesOnDesktop = false;
          ShowHardDrivesOnDesktop = false;
          ShowMountedServersOnDesktop = true;
          ShowRemovableMediaOnDesktop = true;
          _FXSortFoldersFirst = true;
          # When performing a search, search the current folder by default
          FXDefaultSearchScope = "SCcf";
        };

        "com.apple.desktopservices" = {
          # Avoid creating .DS_Store files on network or USB volumes
          DSDontWriteNetworkStores = true;
          DSDontWriteUSBStores = true;
        };

        "com.apple.spaces" = {
          "spans-displays" = 0; # Display have seperate spaces
        };

        "com.apple.WindowManager" = {
          EnableStandardClickToShowDesktop = 0; # Click wallpaper to reveal desktop
          StandardHideDesktopIcons = 0; # Show items on desktop
          HideDesktop = 0; # Do not hide items on desktop & stage manager
          StageManagerHideWidgets = 0;
          StandardHideWidgets = 0;
        };

        "com.apple.screencapture" = {
          location = "~/Downloads";
          type = "png";
        };

        "com.apple.AdLib" = {
          allowApplePersonalizedAdvertising = false;
        };

        # Prevent Photos from opening automatically when devices are plugged in
        "com.apple.ImageCapture".disableHotPlug = true;

        "com.apple.print.PrintingPrefs" = {
          # Automatically quit printer app once the print jobs complete
          "Quit When Finished" = true;
        };

        "com.apple.SoftwareUpdate" = {
          AutomaticCheckEnabled = true;
          # Check for software updates daily, not just once per week
          ScheduleFrequency = 1;
          # Download newly available updates in background
          AutomaticDownload = 1;
          # Install System data files & security updates
          CriticalUpdateInstall = 1;
        };
        "com.apple.TimeMachine".DoNotOfferNewDisksForBackup = true;

        # Turn on app auto-update
        "com.apple.commerce".AutoUpdate = true;
      };

      ".GlobalPreferences" = {
        "com.apple.sound.beep.sound" = "/System/Library/Sounds/Funk.aiff";
      };

      dock = {
        autohide = true;
        orientation = "right";
        launchanim = false;
        show-process-indicators = true;
        show-recents = false;
        static-only = true;
      };

      finder = {
        AppleShowAllExtensions = true;
        ShowPathbar = true;
        FXEnableExtensionChangeWarning = false;
      };

      trackpad = {
        Clicking = true;
        TrackpadThreeFingerDrag = true;
      };
    };

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  system.activationScripts.extraUserActivation.text = ''
    # disable Command Meta D Dict lookup
    defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'
  '';
  # Enable Nix-Darwin services
  # services = {
  #   nix-daemon.enable = true;
  #   bluetooth.enable = true;
  # };

  # # Enable Homebrew package management

  # # Specify additional packages to be installed
  # environment.systemPackages = with pkgs; [
  #   vim
  #   git
  #   htop
  # ];

  # # User-specific settings
  # users.users = {
  #   ${username} = {
  #     isNormalUser = true;
  #     home = "/Users/${username}";
  #     extraGroups = [ "wheel" "admin" ];
  #     shell = pkgs.zsh;
  #   };
  # };

  # # Enable ZSH
  # programs.zsh.enable = true;

  # # Set some global environment variables
  # environment.variables = {
  #   EDITOR = "vim";
  #   PAGER = "less";
  # };

  # # Example of setting some macOS defaults
  # nix-darwin.extra.etc = ''
  #   defaults write com.apple.Dock autohide -bool true
  #   defaults write com.apple.finder ShowPathbar -bool true
  #   killall Dock
  #   killall Finder
  # '';
}
