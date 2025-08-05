# Basic requirements to set up a development environment
{ pkgs ? import <nixpkgs> {} }:
  with pkgs;
  let
    libffi_static =
      libffi.overrideAttrs (old: { dontDisableStatic = true; });
    bzip2_static =
      bzip2.overrideAttrs (old: { dontDisableStatic = true; });
    gmp_static =
      gmp.overrideAttrs (old: { dontDisableStatic = true; });
    lzma_static =
      xz.overrideAttrs (old: { dontDisableStatic = true; });
  in mkShell {
    nativeBuildInputs = [
        cabal-install
        ghc
        haskell-language-server
        haskellPackages.fourmolu
        haskellPackages.cabal-fmt
        hlint
        zlib.dev
        git
        bzip2_static
        bzip2_static.dev
        libffi_static.dev
        gmp_static.dev
        lzma_static.dev
    ] ++ lib.optionals stdenv.isLinux [
        zlib.static
        glibc.dev
        glibc.static
    ];
    shellHook = ''
      export LANG=C.UTF-8
    '';
}
