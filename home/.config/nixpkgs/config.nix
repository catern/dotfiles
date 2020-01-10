{
  allowUnfree = true;
  packageOverrides = pkgs: with pkgs; rec {
    my = {
      emacs = pkgs.emacsWithPackages (epkgs: (with epkgs; [
        auctex
        better-defaults
        circe
        csv-mode
        cyberpunk-theme
        direnv
        ess
        excorporate
        geiser
        ggtags
        graphviz-dot-mode
        htmlize
        intero
        magit
        nix-mode
        notmuch
        nyan-mode
        restclient
        rust-mode
        scala-mode
        slime-volleyball
        toml-mode
        tuareg
        undo-tree
        yaml-mode
      ]));
    };
  };
}
