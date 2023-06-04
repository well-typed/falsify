{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
    in
    flake-utils.lib.eachSystem systems (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        inherit (nixpkgs.lib) composeManyExtensions;
        inherit (pkgs.haskell.lib) packageSourceOverrides;

        localPackages = packageSourceOverrides {
          falsify = ./lib;
        };

        hspkgs = pkgs.haskellPackages.extend localPackages;
      in {
        inherit pkgs hspkgs;

        devShells.default = hspkgs.shellFor {
          packages = p: [
            p.falsify
          ];

          withHoogle = true;

          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.haskellPackages.cabal-fmt
            pkgs.ghcid
            hspkgs.haskell-language-server
            pkgs.haskellPackages.fourmolu

            # Other
            pkgs.nixpkgs-fmt
          ];
        };
    });
}
