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
let
  encryption = with pkgs; [
    gnupg
    pcsc-tools
  ];
  networking = with pkgs; [
    openvpn
  ];
in
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    (inputs.nixos-apple-silicon + "/apple-silicon-support/modules")
    (inputs.home-manager + "/nixos")
    ./../modules/emacs.nix
    ./../modules/sway.nix
    ./../modules/jupyter.nix
    ./../modules/users.nix
    ./../modules/desktop
    ./../modules/eris.nix
    #    ./../modules/private.nix
    #./configuration.nix
  ];
  T.desktop.enable = true;
  T.eris.enable = true;
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

  
  nix.settings.experimental-features = "nix-command flakes";

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
  networking.useNetworkd = true;
  services.resolved.enable = true;

  networking.nameservers = [
    "1.1.1.1"
    "172.16.101.101"
  ];
  systemd.network.enable = true;

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

    # Enable touchpad support (enabled default in most desktopManager).
    libinput.enable = true;

    pipewire = {
      wireplumber = {
        enable = true;
        configPackages = [
          (pkgs.writeTextDir "share/wireplumber/wireplumber.conf.d/10-bluez.conf" ''
            monitor.bluez.properties = {
              bluez5.roles = [ a2dp_sink a2dp_source bap_sink bap_source hsp_hs hsp_ag hfp_hf hfp_ag ]
              bluez5.codecs = [ sbc sbc_xq aac ]
              bluez5.enable-sbc-xq = true
              bluez5.hfphsp-backend = "native"
            }
          '')
        ];

      };
    };

    yggdrasil = {
      enable = true;
      group = "wheel";
      settings = {
        # Find public peers here
        # https://publicpeers.neilalexander.dev/
        Peers = [
          "tls://vpn.itrus.su:7992"
        ];
      };
    };

    locate.enable = true;

    fail2ban.enable = true;
  };

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  programs.direnv = {
    enable = true;
    loadInNixShell = true;
  };

  programs.noisetorch.enable = true;
  services.udev.packages = [ pkgs.yubikey-personalization ];
  services.pcscd.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  environment = {
    shellAliases = {
      "nixos-build" = "cd ~/Sources/tree-3/ && nix-build -A nodes.m1-linux.config.system.build.toplevel";
      sw = "sudo ~/Sources/tree-3/result/bin/switch-to-configuration switch";
    };
    etc = {
      "libinput/local-overrides.quirks".text = ''
        [Apple Internal Keyboard / Trackpad]
        MatchName=Apple Internal Keyboard / Trackpad
        MatchUdevType=touchpad
        ModelAppleTouchpad=1
      '';
    };
  };
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages =
    with pkgs;
    [
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
      yggdrasil
      dnsutils
      htop
      tree
      bc # using it to calculate battery
      kitty
      zathura
      jq
      rustup
      openssl
      openssl_3_3
      pkg-config
      rnnoise-plugin

      pavucontrol
      coppwr

      gnucash
      gcc

      guile
      recutils
      sioyek
      ktorrent

      thunderbird
      ispell

      mattermost-desktop

      yubioath-flutter
      yubikey-manager

      btrfs-progs
      cryptsetup
      plan9port
      eris-go
    ]
    ++ encryption
    ++ networking;

  networking.hosts = {
    "100.64.24.3" = [ "calibre.xps13.ts.someonex.net" ];
    "100.64.24.12" = [ "hv0.ts.probabilistic.ru" ];
    "95.165.26.135" = [ "d1.hosts.aziz.fyi" ];
  };
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  systemd.mounts =
    let
      mkBtrfsVol =
        {
          mountPath,
          options ? "compress=zstd,noatime",
          isRoot ? false,
        }:
        let
          ## this is required as per systemd.mount (1) the name of the
          ## units should refelect is actual path and escaped properly
          # mountPath = lib.normalizePath mountPath;
          # name = lib.concatStringsSep "-" (lib.splitString "/" mountPath);
          # we assume the is the last dir of the path
          subvolName = lib.pipe mountPath [
            (lib.splitString "/")
            (lib.remove "")
            lib.last
          ];

          subvol = if isRoot then "" else "subvol=${subvolName}s,";
        in
        {
          enable = true;
          mountConfig = {
            DirectoryMode = 775;
          };
          # no need to do this since its handled by nix
          #name = "${name}.mount";
          options = "${subvol}${options}";
          type = "btrfs";
          what = "/dev/mapper/enc";
          where = mountPath;
        };
    in
    [
      (mkBtrfsVol {
        mountPath = "/tank";
        options = "noatime";
        isRoot = true;
      })
      (mkBtrfsVol { mountPath = "/tank/src/"; })
      (mkBtrfsVol { mountPath = "/tank/docs/"; })
      (mkBtrfsVol { mountPath = "/tank/eris/"; })
    ];
  # List services that you want to enable:
  systemd.network.networks."10-wlan" = {
    matchConfig.Name = "wlan0";
    DHCP = "yes";
    dhcpV4Config = {
      # UseHostname = false;
      UseDNS = "no";
      UseRoutes = "yes";
      # UseDomains = false;
      # UseNTP = false;
    };
    # dhcpV6Config = {
    #   UseHostname = false;
    #   UseDNS = false;
    #   UseNTP = false;
    # };
  };
  # systemd.network.config.dhcpV4Config = {
  #     UseHostname = "no";
  #     UseDNS = "no";
  #     UseDomains = "no";
  #     UseNTP = "no";
  #   };

  # systemd.network.config.dhcpV6Config = {
  #     UseHostname = false;
  #     UseDNS = false;
  #     UseNTP = false;
  #   };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh = {
	settings.PasswordAuthentication = false;
	settings.KbdInteractiveAuthentication = false;
  };

  networking.resolvconf.enable = false;
  networking.wireless.iwd = {
    enable = true;
    settings.General.EnableNetworkConfiguration = true;
    settings.General.AlwaysRandomizeAddress = true;
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = true;

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
