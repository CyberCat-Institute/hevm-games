{
  description = "Opt-in Stack Flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    solc = {
      url = "github:hellwolf/solc.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hevm = {
      url = "github:ethereum/hevm";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, solc, hevm }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            solc.overlay
            hevm.overlay
          ];
        };
        hPkgs = pkgs.haskell.packages."ghc963"; # need to match Stackage LTS version from stack.yaml resolver
        devTools = [
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          pkgs.zlib # External C library needed by some Haskell packages
          pkgs.libff
          pkgs.git
          pkgs.llvmPackages.clang # Use Clang instead of GCC
          pkgs.secp256k1
          stack-wrapped
          (solc.mkDefault pkgs pkgs.solc_0_8_26)
          pkgs.hevm
        ];
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack"; # will be available as the usual `stack` in terminal
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = devTools;
          # Make external Nix c libraries like zlib known to GHC, like
          # pkgs.haskell.lib.buildStackProject does
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
          CC = "${pkgs.llvmPackages.clang}/bin/clang";
          GCC = "${pkgs.llvmPackages.clang}/bin/clang"; # Add this for symbolic purposes
          shellHook = ''
            ln -sf ${pkgs.llvmPackages.clang}/bin/clang $PWD/gcc
            export PATH=$PWD:$PATH
          '';
        };
      });
}
