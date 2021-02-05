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
    boot.loader.grub.device = "/dev/sdb"; # or "nodev" for efi only

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  
  # fileSystems = [
    # Mount the root file system
    #
    # { mountPoint = "/";
    #   device = "/dev/disk/by-uuid/943d2f5a-b4e9-4ffd-886c-e5285f43a316";
    #   fsType = "ext4";
    # }
    # { mountPoint = "/home/darius";
    #   device = "/dev/disk/by-uuid/1D9C-A577";
    #   fsType = "vfat";
    #   options = ["uid=1000"];
    # }
  #];

  # Select internationalisation properties.
    i18n = {
      consoleFont = "lat9w-16";
      consoleKeyMap = "us";
      defaultLocale = "en_US.UTF-8";
      supportedLocales = ["lt_LT.UTF-8/UTF-8" ];
    };

  
  # Set your time zone.
    time.timeZone = "Europe/Vilnius";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      wget
      wgetpaste
      mutt
      newsbeuter
      libreoffice
      elinks
      git
      kde4.plasma-nm
      kde4.kdeutils
      kde4.applications
      vim
      chromium
    ];

  # Enable the OpenSSH daemon.
    services.openssh.enable = true;

  # Enable CUPS to print documents.
    services.printing.enable = true;

  # Enable the X11 windowing system.
    services.xserver.enable = true;
    services.xserver.layout = "us";
    services.xserver.xkbOptions = "eurosign:e";

  # Enable the KDE Desktop Environment.
    services.xserver.displayManager.kdm.enable = true;
    services.xserver.desktopManager.kde4.enable = true;
    services.xserver.windowManager.xmonad.enable = true;
    services.xserver.synaptics.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.extraUsers.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };

    users.extraUsers.darius = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" "netdev" ];
      uid = 1000;
  #   initialPassword = "start";
    };

  # Networking
    networking.networkmanager.enable = true;

  # The NixOS release to be compatible with for stateful data such as databases.
    system.stateVersion = "16.09";

}
