# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  unstable = import <unstable> {};
in 
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

  # Mozzila
  nixpkgs.overlays =
    let
      # Change this to a rev sha to pin
      moz-rev = "master";
      moz-url = builtins.fetchTarball { url = "https://github.com/mozilla/nixpkgs-mozilla/archive/${moz-rev}.tar.gz";};
      nightlyOverlay = (import "${moz-url}/firefox-overlay.nix");
    in [
      nightlyOverlay
    ];
  

environment.systemPackages = with pkgs; [
      wget
      # (import /home/darius/.emacs.d/emacs.nix { inherit pkgs; })
      emacs
      libreoffice
      # firefox
      latest.firefox-nightly-bin
      ark
      xineLib
      xineUI
      pwsafe
      networkmanager_dmenu
      htop
      hugo
      ledger
      lynx
      # w3m # removed due multiple vulnerabilities
      chromium
      discord
      gitFull
      # firejail
      xclip #Tool to access the X clipboard from a console application
      gimp
      scribus
      syncthing
      mplayer
      smplayer #graphical frontend for mplayer
      ntfs3g #to read windows formated usb flash drives
      android-studio
      multimc
      minecraft
      lightspark #Open source Flash Player implementation
      gitter
      gzip
      zip
      unzip
      pandoc
      bashmount
      wesnoth
      at-spi2-core # install after switch to 20.09
      okular
      kbdd
      kcalc
      ispell
      gnumake 
      aspell
      aspellDicts.lt
      coreutils # for ispell-lt
      findutils # for ispell-lt
      python3 # ispell-lt make wants it more than python 2
      curl #mainly for emacs elfeed
      vulnix
      webcamoid
      unstable.android-tools
      fishnet # Distributed Stockfish analysis for lichess.org
      imagemagick # For creating emacs image thumbnails
      gparted
      mtools # for reading fat32, for some reason it was not included in 21.05 ISO image
      openvpn
      playerctl # for media keys
      xorg.xev # for maping media keys
      mullvad-vpn
      qbittorrent
      # libstdcxx5 #hope firefox-nightly will wok with it
      skanlite
      image_optim
      libjpeg
    ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:
    services.syncthing = {
      enable = true;
      user = "darius";
      dataDir = "/home/darius/.config/syncthing";
    };

  #  services.openvpn.servers = {
  #    teismasVPN  = { config = '' config /root/nixos/openvpn/teismas_gw.ovpn ''; };
  #  };
    
    
    security.chromiumSuidSandbox.enable = true;
  
  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
    networking.firewall.allowedTCPPorts = [ 22 80 443 ];
    networking.firewall.allowedUDPPorts = [ 1194 1325 ]; # for OpenVPN
  # Or disable the firewall altogether.
    # networking.firewall.enable = false;

    networking.firewall.checkReversePath = "loose"; # for Mullvad
      
  # Mullvad VPN
    networking.iproute2.enable = true;
    services.mullvad-vpn.enable = true;

  # Enable CUPS to print documents.
     services.printing.enable = true;
     services.printing.drivers = [ pkgs.cupsBjnp pkgs.samsung-unified-linux-driver ];

  # Scanner
    hardware.sane.enable = true;
     
     
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
    nixpkgs.config.permittedInsecurePackages = [
        "python2.7-Pillow-6.2.2"
    ];


  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
    system.stateVersion = "20.03"; # Did you read the comment?    
}
