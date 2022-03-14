[
  (import ./emacs-overlay)
  (self: super: {
    my = {
      base_emacs = (super.emacsUnstable.override {
          withGTK2 = false;
          withGTK3 = false;
          withMotif = false;
      }).overrideAttrs (_: {
          src = builtins.fetchGit /home/sbaugh/.local/src/emacs;
      });
      emacs = (
        super.emacsPackagesFor self.my.base_emacs
      ).emacsWithPackages (epkgs: (with epkgs; [
        auctex
        better-defaults
        circe
        csv-mode
        cyberpunk-theme
        dumb-jump
        envrc
        ess
        # excorporate
        forge
        # geiser
        ggtags
        graphviz-dot-mode
        htmlize
        magit
        mentor
        nix-mode
        notmuch
        nyan-mode
        restclient
        rust-mode
        scala-mode
        slime-volleyball
        telega
        toml-mode
        # tuareg
        undo-tree
        yaml-mode
      ]));
      multimc = super.multimc.overrideAttrs (_: {
        src = super.fetchFromGitHub {
          owner = "Ponywka";
          repo = "MultiMC5-with-offline";
          rev = "56a2bbce0c1250c97f1749f0c55e5b4ed65a7fa6";
          sha256 = "0d1a64ivvdsl7yrmp4cgm36hmkrgnrjjh0vfjwrfpiynszdk0lbc";
          fetchSubmodules = true;
        };
      });
      bitlbee = super.bitlbee.override { plugins = [ super.purple-matrix ]; };
      packages = super.buildEnv {
        name = "my-packages";
        paths = with super; [
          self.my.emacs
          # self.my.multimc
          atool
          bind
          btrfs-progs
          # citrix_workspace
          cloc
          coccinelle
          detox
          direnv
          dmenu
          dunst
          evince
          file
          gdb
          gftp
          gimp
          gitFull
          glibcInfo
          global
          # gnumake
          gnutls
          google-chrome
          graphviz
          htop
          i3lock
          imagemagick
          inetutils
          inkscape
          ispell
          isync
          iw
          jq
          kazam
          libreoffice
          libxslt
          # linkchecker
          lorri
          lsof
          man-pages
          mercurial
          moreutils
          mosh
          mpv
          msmtp
          nethogs
          notmuch
          num-utils
          openssl
          pavucontrol
          pciutils
          procps
          psmisc
          pup
          python3
          pwgen
          p7zip
          remmina
          rtorrent
          rxvt-unicode
          screenkey
          scrot
          socat
          sxiv
          texlive.combined.scheme-full
          tree
          ucspi-tcp
          units
          unrar
          unzip
          usbutils
          wmname
          xclip
          xmonad-with-packages
          xorg.xev
          xorg.xmodmap
          xonotic-sdl
          xsel
          xxd
          texinfoInteractive
        ];
        # pathsToLink = [ "/share/man" "/share/doc" "/share/info" "/bin" "/etc" ];
        extraOutputsToInstall = [ "man" "doc" "info" ];
        postBuild = ''
          shopt -s nullglob
          for i in $out/share/info/*.info $out/share/info/*.info.gz; do
              $out/bin/install-info $i $out/share/info/dir
          done
        '';
      };
    };
  })
]
