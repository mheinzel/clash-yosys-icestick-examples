{
  packageOverrides = super: let self = super.pkgs; in
  {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ghc844 = super.haskell.packages.ghc844.extend (
          newGhc844Packages: oldGhc844Packages: rec {
            singletons = newGhc844Packages.callPackage ./nix/overrides/singletons-2.4.1.nix {};
          });
      };
    };
  };
}
