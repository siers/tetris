{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

mkShell {
  inputsFrom = [ (callPackage (import ./default.nix) {}).env ];
  buildInputs = [
    haskell-language-server
    cabal-install
  ];
}
