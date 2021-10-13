{ sourcesFile ? ./sources.json
, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, pkgs ? import sources.nixpkgs { }
}:
let 
  npmlock2nix = pkgs.callPackage sources.npmlock2nix { };
in
rec {
  tests = npmlock2nix.build {
    src = ../liquidity-bridge-eth;
    installPhase = "eslint . --max-warnings 0 --ext .ts && mkdir -p $out";
    buildCommands = [];
    HOME="./";
  };
}