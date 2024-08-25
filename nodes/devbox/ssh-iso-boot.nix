{
  config,
  pkgs,
  modulesPath,
  lib,
  ...
}:

let
  image = pkgs.nixos {
    imports = [ (modulesPath + "/installer/cd-dvd/iso-image.nix") ];
    isoImage = {
      makeEfiBootable = true;
    };
    users.users.root.hashedPassword = "";
    users.users.root.openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIApJDbTapinblUjkGSXlRrs8h3NL5HPOlHTqGpE5IkJN aziz@nixos"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM8oas+5PADHvsYSEYq2Dy9jPHe6KOeL2q3KVr6gwXU8 someone/prob solutions"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMPlMI/dPhPSsDqyyoi6+ELDgPGROm8I/SUlkEB3kIBE serge@d1"
    ];
    systemd.network.enable = true;
    systemd.network.networks."10-eth" = {
      matchConfig.type = "ether";
      networkConfig.DHCP = "yes";
      linkConfig.RequiredForOnline = "routable";
    };
    # systemd.network.networks."09-eno1-static" = {
    #   matchConfig.MACAddress = "3c:ec:ef:3a:a7:4a";
    #   DHCP = "yes";
    #   address = [ "10.23.4.13/24" ];
    #   routes = [
    #     { Destination = "10.23.4.1/24"; }
    #     { Destination = "10.10.9.1/24"; }
    #   ];
    # };

    services.openssh.enable = true;

    environment.systemPackages = with pkgs; [
      clevis
      cryptsetup
      efibootmgr
      gptfdisk
      hdparm
      parted
      pciutils
      pv
      util-linux
    ];

    system.stateVersion = "24.05";
  };
  inherit (image.config.system) build;
in
{
  users.users.ssh-iso-boot = {
    isNormalUser = true;
    hashedPassword = "$y$j9T$QEhx6tE9tt.rlSidIgMpq1$U3K0mXHF.yAJEY5uB6ibMuZb1mpfBh/eAdc98jMa.E3";
    homeMode = "750";
  };
  systemd.tmpfiles.settings."10-ssh-iso" = {
    "/home/ssh-iso-boot/image.iso"."L+" = {
      user = "ssh-iso-boot";
      argument = "${build.isoImage}/iso/nixos.iso";
    };
  };

  services.samba.enable = true;
  services.samba.openFirewall = true;
  services.samba.shares = {
    "ssh-iso-boot" = {
      path = "${build.isoImage}/iso/";
      "read only" = true;
      browseable = true;
      "guest ok" = "yes";
    };
  };
}
