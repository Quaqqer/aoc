{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  packages = with pkgs; [
    (python310.withPackages (pyPkgs: with pyPkgs; [ aocd pygame numpy ]))
    (ghc.withPackages (hpkgs:
      with hpkgs; [
        advent-of-code-api
        split
        regex-tdfa
        raw-strings-qq
      ]))
  ];
}
