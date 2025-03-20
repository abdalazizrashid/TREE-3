{
  imports = [
    # ./uvm # microvms
    ./blackhole-links.nix # wireguard

    # netboot options
    # ./ipxe.nix
    # ./ssh-iso-boot.nix
  ];
  networking.firewall.interfaces.wgbh0.allowedTCPPort = [ 7654 ];
  # services.tang.enable = true;
  # services.tang.ipAddressAllow = [ "10.23.4.0/24" ];
  # services.tailscale.enable = true;
  users.users.serge = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM8oas+5PADHvsYSEYq2Dy9jPHe6KOeL2q3KVr6gwXU8 someone/prob solutions"
    ];
  };
}
