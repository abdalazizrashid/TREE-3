{config, lib, pkgs, ...}:
{
  
  # Enable the X11 windowing system.
#  services.xserver = {
#  	enable = true;
#	desktopManager = {
#		xterm.enable = false;
#		xfce =
#		 {
#		 enable = true;
#		noDesktop = true;
#		enableXfwm = false;
#		};
#	};
#	displayManager = { defaultSession = "none+i3";};
#	windowManager.i3 = {
#	enable = true;
#	extraPackages =
#		[
#			pkgs.dmenu
#			pkgs.i3status
#			pkgs.i3lock
#		];
#	};
  #	};
  programs.sway = {
    # Install the packages from nixpkgs
    enable = true;
    # Whether to enable XWayland
    xwayland.enable = true;
    wrapperFeatures.gtk = true;
  };  


}
