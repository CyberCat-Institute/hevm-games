{
  description = "Opt-in Stack Flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    solc = {
      url = "github:hellwolf/solc.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    lido-contracts = {
      url = "github:lidofinance/dual-governance?dir=contracts";
      flake = false;
    };
    npmlock2nix = {
      url = "github:nix-community/npmlock2nix";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-utils, solc, lido-contracts, npmlock2nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            solc.overlay
            (final: prev: {
              npmlock2nix = import npmlock2nix { pkgs = final; };
            })
          ];
        };

        # Create node_modules with OpenZeppelin
        node_modules = pkgs.npmlock2nix.node_modules {
          src = ./.;
          packageJson = pkgs.writeText "package.json" (builtins.toJSON {
            name = "opt-in-stack";
            dependencies = {
              "@openzeppelin/contracts" = "^4.9.3";
            };
          });
          packageLockJson = pkgs.writeText "package-lock.json" (builtins.toJSON {
            name = "opt-in-stack";
            lockfileVersion = 3;
            requires = true;
            packages = {
              "" = {
                name = "opt-in-stack";
                dependencies = {
                  "@openzeppelin/contracts" = "^4.9.3";
                };
              };
              "node_modules/@openzeppelin/contracts" = {
                version = "4.9.3";
                resolved = "https://registry.npmjs.org/@openzeppelin/contracts/-/contracts-4.9.3.tgz";
                integrity = "sha512-He3LieZ1pP2TNt5JbkPA4PNT9WC3gOl7LwZzjrLAFZZRanFuESoYKOB0I+6cTzWUiHnUbeXsuSXryR5Ny/Mz2A==";
              };
            };
          });
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
          pkgs.nodejs
          pkgs.nodePackages.npm
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
          shellHook = ''
            ln -sf ${pkgs.llvmPackages.clang}/bin/clang $PWD/gcc
            export PATH=$PWD:$PATH

            # Set up node_modules with OpenZeppelin
            ln -sf ${node_modules}/node_modules .
            export NODE_PATH=${node_modules}/node_modules
            export PATH=${node_modules}/node_modules/.bin:$PATH

            # Link Lido contracts
            ln -sfn ${lido-contracts} lido
            cp -r $PWD/lido/contracts/* $PWD

            # Create symlink for OpenZeppelin at root if node_modules exists
            mkdir "@openzeppelin"
            if [ -d "node_modules/@openzeppelin" ]; then
              ln -sf $PWD/node_modules/@openzeppelin/* $PWD/@openzeppelin/
            fi
          '';
        };
      });
}
