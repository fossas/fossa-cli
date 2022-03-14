# Basic requirements to set up a development environment
{ pkgs ? import <nixpkgs> {} }:
  with pkgs;
  mkShell {
    nativeBuildInputs = [
        bzip2
        cabal-install
        ghc
        haskellPackages.fourmolu
        hlint 
        xz
        zlib
   ];
}