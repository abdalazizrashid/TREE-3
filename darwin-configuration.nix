{ config, pkgs, ... }:

{
  # System name
  system.name = "my-mac";

  # Enable Nix-Darwin services
  services = {
    nix-daemon.enable = true;
    bluetooth.enable = true;
  };

  # Enable Homebrew package management
  homebrew.enable = true;

  # Specify additional packages to be installed
  environment.systemPackages = with pkgs; [
    vim
    git
    htop
  ];

  # User-specific settings
  users.users = {
    myuser = {
      isNormalUser = true;
      home = "/Users/myuser";
      extraGroups = [
        "wheel"
        "admin"
      ];
      shell = pkgs.zsh;
    };
  };

  # Enable ZSH
  programs.zsh.enable = true;

  # Set some global environment variables
  environment.variables = {
    EDITOR = "vim";
    PAGER = "less";
  };

  # Example of setting some macOS defaults
  nix-darwin.extra.etc = ''
    defaults write com.apple.Dock autohide -bool true
    defaults write com.apple.finder ShowPathbar -bool true
    killall Dock
    killall Finder
  '';
}
