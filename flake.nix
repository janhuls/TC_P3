# This file provides support for `nix build`.
# Using nix is optional.
# `cabal build` should work too, but is less reliable.
{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/25.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem
    (system: let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskellPackages; # Fixes GHC & library versions

        pkg = hpkgs.developPackage {root = ./.;};

        dev-shell = hpkgs.shellFor {
          packages = _: [pkg];
          nativeBuildInputs = [
              # For VSCode IDE integration
              pkgs.haskell-language-server
              # For old-fashioned `cabal build`
              pkgs.cabal-install
              pkgs.ghc
              # For prettifying code
              hpkgs.fix-whitespace
              # For calling the SSM emulator
              pkgs.jre8
            ];
          # Include an offline-usable `hoogle` command
          # pre-loaded with all the haskell dependencies
          withHoogle = true;
        };

      in {
        # Entry point for `nix build`, `nix shell`, and `nix run`
        packages.default = pkg;
        # Entry point for `nix develop`, provides haskell dev tools
        devShells.default = dev-shell;
      }
    );
}
