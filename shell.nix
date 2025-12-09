{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  packages = with pkgs; [
    z3

    (python313.withPackages (
      pyPkgs: with pyPkgs; [
        aocd
        pygame
        numpy
        scipy
        z3-solver
        networkx
        shapely
      ]
    ))
  ];
}
