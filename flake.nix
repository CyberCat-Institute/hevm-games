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
             (final: prev: {
              haskell = prev.haskell // {
                packages = prev.haskell.packages // {
                  ghc963 = prev.haskell.packages.ghc963.override {
                    overrides = self: super: {
                      hevm = self.callCabal2nix "hevm" (pkgs.fetchFromGitHub {
                        owner = "ethereum";
                        repo = "hevm";
                        rev = "0.51.3"; # You can update this to the version you need
                        sha256 = "sha256-T0YLVtCVtbOSNX4rjGK42EEvLsUOPCDzh0UbDn9nQP4="; # Update this hash
                      }) {};
                    };
                  };
                };
              };
            })
          ];
        };

        hPkgs = pkgs.haskell.packages."ghc963";

        devTools = [
          hPkgs.ghc
          pkgs.zlib
          pkgs.libff
          pkgs.git
          pkgs.llvmPackages.clang
          pkgs.secp256k1
          stack-wrapped
          (solc.mkDefault pkgs pkgs.solc_0_8_26)
        ];
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
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
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath devTools;
          CC = "${pkgs.llvmPackages.clang}/bin/clang";
          GCC = "${pkgs.llvmPackages.clang}/bin/clang";
          shellHook = ''
            ln -sf ${pkgs.llvmPackages.clang}/bin/clang $PWD/gcc
            export PATH=$PWD:$PATH
          '';
        };
      });
}

