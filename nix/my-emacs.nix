{fetchFromGitHub, ...}:

let
  master-nixpkgs = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "4f5918cd2e11082f33b3ce6a1bdcd5f70c68d3c2";
    sha256 = "0myz5jdcgcqll1mi81659a5i304r26lz97jrnc0a14gjhc48cilr";
  }) {};
in with master-nixpkgs;
let emacs = (emacsPackagesNgGen emacs25pre).emacsWithPackages (p: with p; [
      helm
      projectile
      undo-tree
      helm-projectile
      magit
      f s dash anaphora request
      yasnippet
      erlang
      nix-mode
      paredit
      ws-butler
      keyfreq
      mu4e-maildirs-extension
      zenburn-theme
      smart-mode-line
      pt
      highlight-parentheses
      zoom-frm
      elisp-slime-nav
      key-chord
      htmlize
      auto-complete eproject popup auto-highlight-symbol # edts deps
      intero
      yaml-mode
    ]);
in emacs
