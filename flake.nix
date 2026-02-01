{
  description = "Lune";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell
            ghc
            cabal-install
            haskell-language-server

            # Native dependencies for Haskell libs
            zlib
            pkg-config
          ];

          shellHook = ''
            echo "Lune development shell"
            echo "  ghc:   $(ghc --version)"
            echo "  cabal: $(cabal --version | head -1)"
          '';

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.zlib
          ];
        };
      }
    );
}
