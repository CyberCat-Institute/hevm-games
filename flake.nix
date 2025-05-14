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

      # If you need a specific ref:
      flake = false;  # Tell Nix this input isn't a flake
    };
    foundry.url = "github:shazow/foundry.nix/stable"; # Use stable branch for permanent releases

  };

  outputs = { self, nixpkgs, flake-utils, solc, lido-contracts, foundry }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            solc.overlay
            foundry.overlay
          ];
        };
        hPkgs = pkgs.haskell.packages."ghc963";
        nodeEnv = pkgs.buildEnv {
          name = "node-env";
          paths = [
            pkgs.nodejs
            pkgs.nodePackages.npm
          ];
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
          foundry.defaultPackage.${system}
          nodeEnv
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
            export NODE_PATH="$PWD/node_modules"
            export NPM_CONFIG_PREFIX="$PWD/.npm-global"
            export PATH="$NPM_CONFIG_PREFIX/bin:$PATH"
            mkdir -p $NPM_CONFIG_PREFIX
            npm install
            forge install --no-git OpenZeppelin/openzeppelin-contracts@v4.8.0
            forge install --no-git https://github.com/lidofinance/dual-governance.git 
          '';
        };
      });
}
