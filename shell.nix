{ bootstrap ? import <nixpkgs> { }, compiler ? "ghc843" }:
let
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  # pin nixpkgs to 18.09 so we get the right haskell dep versions
  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    # nixpkgs tag 18.09
    inherit (nixpkgs) rev sha256;
  };

  # We need to override clash-prelude in our packageset to force
  # it to not run doctests (which fail due to deps missing)
  pkgs = import src { config = import ./config.nix; };

  haskell = pkgs.haskell.packages.${compiler};

  # We need clash as both a library and an executable
  ghc = haskell.ghcWithPackages (ps: with ps; [clash-ghc]);
in
  pkgs.mkShell {
    buildInputs = [
      pkgs.yosys
      pkgs.arachne-pnr
      pkgs.icestorm
      pkgs.gnumake
      ghc
      haskell.clash-ghc
    ];
  }
