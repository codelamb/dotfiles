# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
 imports =
   [ # Include the results of the hardware scan.
     ./hardware-configuration.nix
   ];

  
  # Use the GRUB 2 boot loader.
    boot.loader.grub.enable = true;
    boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
    boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only
    networking.networkmanager.enable = true;

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
    i18n = {
      defaultLocale = "lt_LT.UTF-8";
    };

    console.keyMap = "us";
    console.font = "Lat2-Terminus16";
    
  # Set your time zone.
    time.timeZone = "Europe/Vilnius";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
    environment.systemPackages = with pkgs; [
      wget
        # (import /etc/nixo/emacs.nix { inherit pkgs; })
      emacs	
      libreoffice
      firefox
      ark
      xineLib
      xineUI
      pwsafe
      networkmanager_dmenu
      htop
      hugo
      ledger
      lynx
      w3m
      chromium
      discord
      gitFull
      firejail
      xclip #Tool to access the X clipboard from a console application
      gimp
      scribus
      syncthing
      mplayer
      smplayer #graphical frontend for mplayer
      ntfs3g #to read windows formated usb flash drives
      android-studio
      multimc
      lightspark #Open source Flash Player implementation
      gitter
      zip
      unzip
      pandoc
      bashmount
      wesnoth
      at-spi2-core # install after switch to 20.09
      okular
      kbdd
      kcalc
    ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

    security.chromiumSuidSandbox.enable = true;
  
  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
    networking.firewall.allowedTCPPorts = [ 80 443 ];
  # networking.firewall.allowedUDPPorts = [ 631 ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
     services.printing.enable = true;
  #  services.printing.drivers = [ ... ];
    
  # Enable sound.
    sound.enable = true;
    hardware.pulseaudio.enable = true;

  # KDE
    services.xserver.enable = true;
    services.xserver.displayManager.sddm.enable = true;
    services.xserver.desktopManager.plasma5.enable = true;


  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
    services.xserver.libinput.enable = true;
    
  # Define a user account. Don't forget to set a password with ‘passwd’.
    users.users.darius = {
      isNormalUser = true;
      extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
      };
    users.users.juozapas = {
      isNormalUser = true;
    };
    users.users.ausris = {
      isNormalUser = true;
    };

  # Automatic upgrades
    system.autoUpgrade.enable = true;

  # Automatic garbage cleaner
    nix.gc.automatic = true;
    nix.gc.dates = "03:15";

  # EVIL
    nixpkgs.config.allowUnfree = true;
  # nixpkgs.config.allowBroken = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
    system.stateVersion = "20.03"; # Did you read the comment?    
}
