# Open-games HEVM integration

This project integrates open games with HEVM, an execution environement for EVM bytecode.

This allows the game-theoretic analysis of smart contracts

## How to install

For linux, it should be enough to run

    $ stack build


If you require a specific version of solidity, you can use nix and
this overlay: https://github.com/hellwolf/solc.nix

### Mac OS Specific instructions

If you use Mac OS on Apple silicon, you will want to change
the line

```
arch: x86_64
```

in `stack.yaml`, and replace it by

```
arch: aarch64
```

You also need to install solidity via `brew` with

    $ brew install solidity

after that you can build the project with

    $ stack build
