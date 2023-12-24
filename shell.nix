{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  packages = with pkgs; [
    (python311.withPackages (pyPkgs: with pyPkgs; [ aocd pygame numpy z3 ]))
    (ghc.withPackages (hpkgs:
      with hpkgs; [
        advent-of-code-api
        split
        regex-tdfa
        raw-strings-qq
      ]))
    z3
  ];
}
