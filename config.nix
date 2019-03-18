{
  packageOverrides = super: let self = super.pkgs; in
  {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ghc843 = super.haskell.packages.ghc843.extend (
          newGhc843Packages: oldGhc843Packages: rec {
            clash-prelude =
              let oldPkg = oldGhc843Packages.clash-prelude;
               in self.haskell.lib.dontCheck oldPkg;
          });
      };
    };
  };
}
