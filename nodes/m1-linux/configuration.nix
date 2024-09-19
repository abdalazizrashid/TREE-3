# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    (inputs.nixos-apple-silicon + "/apple-silicon-support/modules")
    (inputs.home-manager + "/nixos")
    ./../modules/emacs.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = false;
  boot.resumeDevice = "/dev/disk/by-uuid/212c94ea-9e79-4e58-93ac-63d4d464ddfd";
  boot.kernelParams = [
    "resume_offset=13465600"
    "mem_sleep_default=deep"
  ];
  security.protectKernelImage = false;
  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 32 * 1024;
    }
  ];
  networking.hostName = "afdee1c"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "Europe/Moscow";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.ui
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  hardware = {
    asahi = {
      useExperimentalGPUDriver = true;
      setupAsahiSound = true;
    };
    graphics.enable = true;
    bluetooth.enable = true;
  };
  programs.light.enable = true;

  programs.git = {
    enable = true;

  };

  services = {
    actkbd = {
      enable = true;
      bindings = [
        {
          keys = [ 225 ];
          events = [ "key" ];
          command = "/run/current-system/sw/bin/light -A 5";
        }
        {
          keys = [ 224 ];
          events = [ "key" ];
          command = "/run/current-system/sw/bin/light -U 5";
        }
      ];
    };
    greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd sway";
          user = "greeter";
        };
      };
    };

    tailscale.enable = true;
    logind.extraConfig = ''
      # don’t shutdown when power button is short-pressed
      HandlePowerKey=sleep
    '';
  };

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  programs.direnv = {
    enable = true;
    loadInNixShell = true;
  };
  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.afdee1c = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };
  home-manager.users.afdee1c =
    { pkgs, ... }:
    {
      home.enableNixpkgsReleaseCheck = false;
      home.packages = with pkgs; [
        tree
        htop
        eaglemode
        bc # using it to calculate battery
        fd
        ripgrep
        fzf
        kitty
        zathura
        jq
        rustup
        nixfmt-rfc-style
        signal
      ];
      programs.bash.enable = true;
      programs.git = {
        enable = true;
        userEmail = "ping@aziz.fyi";
        userName = "Aziz";
      };
      # The state version is required and should stay at the version you
      # originally installed.
      home.stateVersion = "24.05";
    };

  environment = {
    shellAliases = {
      "nixos-build" = "cd ~/Sources/tree-3/ && nix-build -A nodes.m1-linux.config.system.build.toplevel";
      sw = "sudo ~/Sources/tree-3/result/bin/switch-to-configuration switch";
    };
    etc = {
      "libinput/local-overrides.quirks".text = ''
        [Apple Keyboard]
        MatchUdevType=touchpad
        MatchDeviceTree=*apple*
        AttrPressureRange=1100:1000'';
    };
  };
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    firefox
    neovim
    cmake
    gnumake
    git
    vim
    wget
    grim # screenshot functionality
    slurp # screenshot functionality
    wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
    mako # notification system developed by swaywm maintainer
    dict
    libtool
    libvterm
    alsa-utils
    mpv
    qrscan
  ];

  networking.hosts = {
    "100.64.24.3" = [ "calibre.xps13.ts.someonex.net" ];
  };
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;
  networking.wireless.iwd = {
    enable = true;
    settings.General.EnableNetworkConfiguration = true;
  };
  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "24.11"; # Did you read the comment?

}
