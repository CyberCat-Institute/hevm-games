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
          ];
        };
        hPkgs = pkgs.haskell.packages."ghc963";

        # Create OpenZeppelin npm package
        openzeppelin-contracts = pkgs.mkYarnPackage {
          name = "openzeppelin-contracts";
          src = pkgs.fetchFromGitHub {
            owner = "OpenZeppelin";
            repo = "openzeppelin-contracts";
            rev = "v5.0.1"; # Replace with desired version
            sha256 = "sha256-YHf6MCdHx2yODw4TLz9dJxrwjpRDPWDjkxD1mpirQEs="; # Update this hash using nix-prefetch-github
          };
          packageJSON = ./package.json;
          yarnLock = ./yarn.lock;
        };

        devTools = [
          hPkgs.ghc
          pkgs.zlib
          pkgs.libff
          pkgs.git
          pkgs.llvmPackages.clang
          pkgs.secp256k1
          stack-wrapped
          (solc.mkDefault pkgs pkgs.solc_0_8_26)
          hevm.packages.${system}.default
          pkgs.nodejs_20
          pkgs.yarn
          openzeppelin-contracts
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

            # Create node_modules if it doesn't exist
            if [ ! -d "node_modules" ]; then
              mkdir -p node_modules
            fi

            # Link OpenZeppelin contracts
            ln -sfn ${openzeppelin-contracts}/libexec/openzeppelin-contracts/node_modules/@openzeppelin node_modules/@openzeppelin
          '';
        };
      });
}
