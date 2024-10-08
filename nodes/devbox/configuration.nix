# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, inputs, ... }:

{
  imports =
    [
    (inputs.sops-nix + "/modules/sops")
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # ../modules/bind.nix
    # ../modules/knot-dns.nix
    #../modules/kea.nix
    ../modules/tailscale.nix
    #./acme.nix
    #./gnome.nix
    #./misc.nix
    ../modules/znc.nix
    # ./probabilistic.ru.nix
    ./l2tp.nix
    ];

    sops.defaultSopsFile = ./secrets/default.yaml;
    sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];

    programs.nix-ld.enable = true;

    nix.settings = {
      experimental-features = [
        "ca-derivations"
        "cgroups"
        "flakes"
        "impure-derivations"
        "nix-command"
      ];
    };

    # Bootloader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    networking.hostName = "d1"; # Define your hostname.
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    # Enable networking
    networking.networkmanager.enable = true;
    networking.networkmanager.dns = "systemd-resolved";
    networking.nameservers = [
        "1.1.1.1" # FIXME: DO NOT USE CLOUDFLARE
    ];

    # Set your time zone.
    time.timeZone = "Europe/Moscow";

    # Select internationalisation properties.
    i18n.defaultLocale = "en_US.UTF-8";

    i18n.extraLocaleSettings = {
      LC_ADDRESS = "ru_RU.UTF-8";
      LC_IDENTIFICATION = "ru_RU.UTF-8";
      LC_MEASUREMENT = "ru_RU.UTF-8";
      LC_MONETARY = "ru_RU.UTF-8";
      LC_NAME = "ru_RU.UTF-8";
      LC_NUMERIC = "ru_RU.UTF-8";
      LC_PAPER = "ru_RU.UTF-8";
      LC_TELEPHONE = "ru_RU.UTF-8";
      LC_TIME = "ru_RU.UTF-8";
    };

    services.openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
      settings.KbdInteractiveAuthentication = false;
      #settings.PermitRootLogin = "yes";
    };	
    users.users.root.openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICX8KUmbAs2AcWar9cji61/+6R8m4aqoHaTKW1kcqg9D aziz@w375262.staff.corp.local"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM8oas+5PADHvsYSEYq2Dy9jPHe6KOeL2q3KVr6gwXU8 someone/prob solutions"
    ];

    # Disable the GNOME3/GDM auto-suspend feature that cannot be disabled in GUI!
    # If no user is logged in, the machine will power down after 20 minutes.
    systemd.targets.sleep.enable = false;
    systemd.targets.suspend.enable = false;
    systemd.targets.hibernate.enable = false;
    systemd.targets.hybrid-sleep.enable = false;


    services.emacs = {
      enable = true;
    };
    # Enable the X11 windowing system.
    services.xserver.enable = false;

    # Enable the GNOME Desktop Environment.
    services.xserver.displayManager.gdm.enable = false;
    services.xserver.desktopManager.gnome.enable = false;

    # Configure keymap in X11
    services.xserver = {
      layout = "us";
      xkbVariant = "";
    };

    # Enable CUPS to print documents.
    services.printing.enable = true;

    # Enable sound with pipewire.
    hardware.pulseaudio.enable = false;
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };
    

    services.xserver.xautolock.time = -1;
    # Enable touchpad support (enabled default in most desktopManager).
    # services.xserver.libinput.enable = true;

    users.groups.nixos-editors = {};

    # Define a user account. Don't forget to set a password with ‘passwd’.
    users.users.aziz = {
      isNormalUser = true;
      description = "aziz";
      extraGroups = [ "calibre-server" "networkmanager" "wheel" "nixos-editors" ];
      packages = with pkgs; [
    	  emacs
        #  thunderbird
        dnsutils
      ];
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICX8KUmbAs2AcWar9cji61/+6R8m4aqoHaTKW1kcqg9D aziz@w375262.staff.corp.local"
      ];
    };
    programs.neovim = {
        enable = true;
        defaultEditor = true;
        configure = {
            customRC = ''
                :set smartindent
                :set expandtab
                :set tabstop=4
                :set shiftwidth=4
                :set numberwidth=4
                :set number
                '';
        };
    };
    programs.tmux = {
        enable = true;
        clock24 = true;
        escapeTime = 100;
        keyMode = "vi";
        newSession = true;
        plugins = with pkgs.tmuxPlugins; [
            sensible
            pain-control
            gruvbox
        ];
    };
    users.users.serge = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      openssh.authorizedKeys.keys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM8oas+5PADHvsYSEYq2Dy9jPHe6KOeL2q3KVr6gwXU8 someone/prob solutions" ];
    };
    programs.mosh.enable = true;
    programs.mosh.openFirewall = true;

    # Allow unfree packages
    nixpkgs.config.allowUnfree = true;

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    environment.systemPackages = with pkgs; [
      #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
      #  wget
      emacs
      vim
      headscale
      git
      nix-output-monitor
      npins
      silver-searcher
      fd
      nixfmt-rfc-style

      dnsutils
      nmap

    ] ++ [
      config.services.headscale.package 
    ] ++ [ pkgs.dnsutils ];
    nix.trustedUsers = [ "root" "@wheel" ];

    systemd.network.enable = true;

    # These have been handled by networkmanager so far, let's not touch them yet
    systemd.network.networks."20-unmanaged" = {
      matchConfig.Name = [ "enp6s0" "enp4s0" ];
      linkConfig.Unmanaged = true;
    };
    systemd.network.wait-online.enable = false; # Enable when migrating from NM

    # # Activation script to set permissions and ownership
    # system.activationScripts.nixosEditorsPermissions = ''
    #   # Ensure the /etc/nixos directory and its files have the correct permissions
    #   chown -R root:nixos-editors /etc/nixos
    #   find /etc/nixos -type d -exec chmod 775 {} \;
    #   find /etc/nixos -type f -exec chmod 664 {} \;
    # '';

    # Some programs need SUID wrappers, can be configured further or are
    # started in user sessions.
    # programs.mtr.enable = true;
    # programs.gnupg.agent = {
      #   enable = true;
      #   enableSSHSupport = true;
      # };

      # List services that you want to enable:

      # FIXME: enable; also use nftables
      networking.firewall.enable = false;

      # This value determines the NixOS release from which the default
      # settings for stateful data, like file locations and database versions
      # on your system were taken. It‘s perfectly fine and recommended to leave
      # this value at the release version of the first install of this system.
      # Before changing this value read the documentation for this option
      # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
      system.stateVersion = "24.05"; # Did you read the comment?

}
