{
  config,
  pkgs,
  modulesPath,
  lib,
  ...
}:

let
  image = pkgs.nixos {
    imports = [
      (modulesPath + "/installer/netboot/netboot-minimal.nix")

    ];
    users.users.root.openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIApJDbTapinblUjkGSXlRrs8h3NL5HPOlHTqGpE5IkJN aziz@nixos"
    ];
    systemd.network.enable = true;
    systemd.network.networks."10-eth" = {
      matchConfig.type = "ether";
      networkConfig.DHCP = "yes";
      linkConfig.RequiredForOnline = "routable";
    };
    services.openssh.enable = true;
    system.stateVersion = "24.05";
  };
  inherit (image.config.system) build;
in
{
  options.recovery-image = lib.mkOption {
    type = with lib.types; attrsOf package;
    default = build;
  };
  config = {
    services.pixiecore = {
      enable = true;
      openFirewall = true;
      dhcpNoBind = true;
      listen = "10.23.4.69";
      # port = ...
      mode = "boot";
      kernel = "${build.kernel}/bzImage";
      initrd = "${build.netbootRamdisk}/initrd";
      cmdLine = "init=${build.toplevel}/init loglevel=4";
      debug = true;
      extraArguments = [
        # "--httpboot-url=http://10.23.4.69/bootx64.efi"
      ];
    };
  };
}
