{fetchFromGitHub, ...}:

let
  master-nixpkgs = import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "4f5918cd2e11082f33b3ce6a1bdcd5f70c68d3c2";
    sha256 = "0myz5jdcgcqll1mi81659a5i304r26lz97jrnc0a14gjhc48cilr";
  }) {};
in with master-nixpkgs; 
(emacsPackagesNgGen emacs25pre).emacsWithPackages (p: with p; [
  helm
  projectile
  helm-projectile
  magit
  f s dash anaphora request
  yasnippet
  erlang
  nix-mode
  paredit
])
