{nixpkgs ? import <nixpkgs> {}, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "hs-libp2p-peer";
  buildInputs = [ stack cabal-install autoreconfHook ];
  inherit ghc;
}
