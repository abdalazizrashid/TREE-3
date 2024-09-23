{
  pkgs,
  libs,
  config,
  ...
}:
let
  username = "afdee1c";
in
{
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users."${username}" = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };
  home-manager.users."${username}" = (import ../../users/aziz/.config/home-manager);
}
