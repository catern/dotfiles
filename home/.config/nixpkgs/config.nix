{
  allowUnfree = true;
  allowBroken = true;
  packageOverrides = pkgs: with pkgs; rec {
    myEmacs = emacsWithPackages (epkgs: (with epkgs; [
      better-defaults
      undo-tree
      cyberpunk-theme
      magit
      circe
      csv-mode
      yaml-mode
      toml-mode
      rust-mode
      nix-mode
      geiser
      restclient
      ess
      intero
      nyan-mode
      slime-volleyball
      ix
      tuareg
      merlin
    ]));
  };
}
