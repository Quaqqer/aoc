{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  packages = with pkgs; [
    z3

    (python3.withPackages (
      pyPkgs: with pyPkgs; [
        aocd
        pygame
        numpy
        scipy
        z3
        networkx
      ]
    ))

    (ghc.withPackages (
      hpkgs: with hpkgs; [
        advent-of-code-api
        split
        regex-tdfa
        raw-strings-qq
      ]
    ))

    haskell-language-server
  ];
}
