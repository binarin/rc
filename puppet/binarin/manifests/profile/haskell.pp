class binarin::profile::haskell {
  ensure_packages(["ghc", "cabal-install", "xorg-dev"], {ensure => latest})
}
