with import ./nix { };
pkgs.npmlock2nix.shell {
    src = ./liquidity-bridge-eth;
    buildInputs = [ pkgs.solc ];
}
