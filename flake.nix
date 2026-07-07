{
  description = "Akshay Srivatsan's personal website";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
  };
  outputs = { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          ghc
          zlib
          cabal-install
          haskell-language-server
          hlint
          ormolu
          haskellPackages.cabal-fmt
        ];
      };
    };
}
