{ nixpkgs ? import <nixpkgs> { config = import ./config.nix; }
, compiler ? "ghc844" 
}:

let
  inherit (nixpkgs) pkgs;
#  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; []);
  clash-ghc = pkgs.haskell.packages.${compiler}.clash-ghc;
in
  pkgs.mkShell {
    buildInputs = [ 
      pkgs.yosys
      pkgs.arachne-pnr
      pkgs.icestorm
#      ghc
      clash-ghc
    ];
  }
