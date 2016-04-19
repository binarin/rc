{ system ? builtins.currentSystem }:
let
  pkgs = import <nixpkgs> { inherit system; };
in rec {
  yandex-disk = pkgs.callPackage ./yandex-disk {};
}
