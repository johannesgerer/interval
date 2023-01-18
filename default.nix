# -*- compile-command: "nix-shell --run 'cabal exec -- ghc-pkg list'"; -*-
{ pkgs ? import (import ./nix/sources.nix {}).nixpkgs {}, sources ? import ./nix/sources.nix {} }:
let 
    minimalCompilation = x: with pkgs.haskell.lib; disableLibraryProfiling (dontHaddock x);
    minSource = self: name: minimalCompilation (self.callCabal2nix name sources.${name} {});
in
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/make-package-set.nix
pkgs.haskellPackages.developPackage {


    root = ./.;
    withHoogle = false;
    returnShellEnv = false;
    overrides = self: _:  {
      yahp              = minSource self "yahp";
      chronos           = minSource self "chronos";
    };
    modifier = with pkgs.haskell.lib; drv:
      disableLibraryProfiling (dontHaddock (addBuildTools drv
        (with pkgs.haskellPackages; [ cabal-install ghcid])));
  }
