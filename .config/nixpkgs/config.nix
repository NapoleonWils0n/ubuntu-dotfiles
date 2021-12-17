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
        csvkit
        dunst
        emacs
        exiftool
        feh
        fzf
        gdal
        gimp
        gpsbabel
        graphviz
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
        oathToolkit
        pandoc
        playerctl
        ripgrep
        scrot
        sox
        shellcheck
        sxhkd
        sxiv
        tig
        tint2
        tmux
        tree
        urlscan
        urlview
        ytfzf
        yt-dlp
        w3m
        zathura
      ];
      pathsToLink = [ "/share/man" "/share/doc" "/share/info" "/share/applications" "/bin" "/etc" ];
      extraOutputsToInstall = [ "man" "doc" "info" "applications" ];
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
