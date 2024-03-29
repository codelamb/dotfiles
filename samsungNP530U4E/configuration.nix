# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
   networking.networkmanager.enable = true; 

  # Set your time zone.
  time.timeZone = "Europe/Vilnius";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp3s0.useDHCP = true;
  networking.interfaces.wlp2s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
    i18n.defaultLocale = "en_US.UTF-8";
    console = {
      font = "Lat2-Terminus16";
      keyMap = "us";
    };

  # Enable the X11 windowing system.
  services.xserver.enable = true;


  # Enable the Plasma 5 Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  # Autologin
  services.xserver.displayManager.autoLogin.enable = true;
  services.xserver.displayManager.autoLogin.user = "VidaMarija";

  # Configure keymap in X11
    services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
    services.printing.enable = true;
    services.printing.drivers = [ pkgs.splix ]; # for xerox workcenter 3119

  # Scanner
    hardware.sane.enable = true;

  # Enable sound.
    sound.enable = true;
    hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
    users.users.VidaMarija = {
      isNormalUser = true;
      extraGroups = [ "wheel" "scanner" "lp" ]; # Enable ‘sudo’ for the user.
    };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
    environment.systemPackages = with pkgs; [
      emacs
      wget
      libreoffice
      firefox
      chromium
      okular
      ark
      gwenview
      xineLib
      xineUI
      vlc
      zoom-us
      zip
      unzip
      kcalc #calculator
      ntfs3g #to read windows formated usb flash drives
      xclip #Tool to access the X clipboard from a console application
      lightspark #Open source Flash Player implementation
      mtools # to read fat32 filesystems
      teams
      webcamoid
      gnome.simple-scan
    ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
    networking.firewall.allowedTCPPorts = [ 80 443 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Evil
    nixpkgs.config.allowUnfree = true;

  # Automatic upgrades
    system.autoUpgrade.enable = true;

  # Automatic garbage cleaner
    nix.gc.automatic = true;
    nix.gc.dates = "03:15";

  # Silent boot
    boot.consoleLogLevel = 0;
    boot.kernelParams = [ "quiet" "udev.log_priority=3" ];
    boot.initrd.verbose = false;

  # Splash screen
    boot.plymouth.enable = true;

  # Security
  boot.loader.systemd-boot.editor = false;

  # Bluetooth
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  hardware.bluetooth.settings = {
  General = {
    Enable = "Source,Sink,Media,Socket";
    };
  };
  hardware.pulseaudio.extraConfig = "
  load-module module-switch-on-connect
  ";

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}

