{
  config,
  host-config,
  pkgs,
  ...
}:
{
  microvm.cloud-hypervisor.extraArgs = [
      "-v"
  ];
  users.users.aziz = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    hashedPassword = "";
    openssh.authorizedKeys.keys = host-config.users.users.aziz.openssh.authorizedKeys.keys ++ [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIApJDbTapinblUjkGSXlRrs8h3NL5HPOlHTqGpE5IkJN aziz@nixos"
    ];
  };
  services.openssh.enable = true;
}
