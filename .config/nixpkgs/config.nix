{
  packageOverrides = pkgs: with pkgs; rec {
    myProfile = writeText "my-profile" ''
      export PATH=$HOME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/sbin:/bin:/usr/sbin:/usr/bin
      export MANPATH=$HOME/.nix-profile/share/man:/nix/var/nix/profiles/default/share/man:/usr/share/man
      export INFOPATH=$HOME/.nix-profile/share/info:/nix/var/nix/profiles/default/share/info:/usr/share/info
    '';
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = [
        (runCommand "profile" {} ''
          mkdir -p $out/etc/profile.d
          cp ${myProfile} $out/etc/profile.d/my-profile.sh
        '')
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
        emacs
        exiftool
        exa
        feh
        ffmpeg_5
        fzf
        gdal
        gimp
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
        mediainfo
        meld
        mpc_cli
        mpd
        mutt
        nnn
        ncdu
        ncmpc
        networkmanagerapplet
        newsboat
        nsxiv
        oathToolkit
        pandoc
        playerctl
        p7zip
        ripgrep
        scrot
        socat
        sox
        shellcheck
        streamlink
        surfraw
        sxhkd
        sxiv
        tig
        tint2
        tmux
        tree
        unzip
        viddy
        urlscan
        urlview
        ytfzf
        yt-dlp
        weechat
        zathura
        zip
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/share/info" "/share/applications" "/share/icons" "/bin" "/etc" ];
      extraOutputsToInstall = [ "man" "doc" "info" "applications" "icons" ];
      postBuild = ''
        if [ -x $out/bin/install-info -a -w $out/share/info ]; then
          shopt -s nullglob
          for i in $out/share/info/*.info $out/share/info/*.info.gz; do
              $out/bin/install-info $i $out/share/info/dir
          done
        fi
      '';
    };
  };
}
