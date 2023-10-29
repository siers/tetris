{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        with nixpkgs.legacyPackages.${system};
        let
          cabal = haskellPackages.callPackage ./cabal.nix {};
        in
          {
            packages.default = cabal;

            devShells.default =
              mkShell {
                inputsFrom = [ cabal.env ];
                buildInputs = [
                  haskell-language-server
                  cabal-install
                  haskellPackages.fourmolu
                ];
              };
            }
      );
}
