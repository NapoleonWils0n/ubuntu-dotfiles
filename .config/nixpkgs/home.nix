{ config, pkgs, ... }:

let

  unstable = import (fetchTarball
    "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
      overlays = [
        (import (builtins.fetchTarball {
          url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
        }))
      ];
    };

in {

    programs.emacs = {
      enable = true;
      package = unstable.emacsPgtkNativeComp;
    };

  home.packages = with pkgs;[
    abook
    apg
    aria
    awscli
    bat
    bc
    curl
    csvkit
    dos2unix
    dunst
    exiftool
    exa
    feh
    git
    gnuplot
    gpsbabel
    graphviz
    gparted
    handbrake
    html-xml-utils
    imagemagick
    jq
    lynx
    libxslt
    mediainfo
    meld
    mpc_cli
    mpd
    mutt
    ncdu
    ncmpc
    newsboat
    nsxiv
    oathToolkit
    pandoc
    playerctl
    prowlarr
    p7zip
    ripgrep
    socat
    sox
    shellcheck
    streamlink
    surfraw
    sxiv
    tig
    tmux
    unzip
    viddy
    urlscan
    urlview
    yt-dlp
    weechat
    zathura
    zip
  ];

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "djwilcox";
  home.homeDirectory = "/home/djwilcox";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
