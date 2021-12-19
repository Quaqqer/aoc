#!/usr/bin/env python3
from collections import defaultdict
from itertools import chain, permutations
from typing import Optional

import numpy as np
from aocd.models import Puzzle
from numpy import matmul

puzzle = Puzzle(2021, 19)
scanners: list[list[np.ndarray]] = [
    [
        np.array([int(v) for v in beacon.split(",")])
        for beacon in scanner.splitlines()[1:]
    ]
    for scanner in puzzle.input_data.split("\n\n")
]

matrices = []
for perm in permutations((0, 1, 2)):
    x, y, z = perm

    for i in [-1, 1]:
        for j in [-1, 1]:
            for k in [-1, 1]:
                vec = [[0, 0, 0], [0, 0, 0], [0, 0, 0]]
                vec[0][x] = i
                vec[1][y] = j
                vec[2][z] = k
                matrices.append(np.array(vec))


def r_intersects(bs1: list[np.ndarray], bs2: list[np.ndarray]) -> Optional[np.ndarray]:
    """
    Intersection between two already rotated scanners.

    :param bs1 list[np.ndarray]: First list of beacons.
    :param bs2 list[np.ndarray]: Second list of beacons.
    :rtype Optional[np.ndarray]: Relative transformation, bs2 - bs1.
    """
    # relative counter
    relatives = defaultdict(int)
    for b1 in bs1:
        for b2 in bs2:
            relative = tuple(b2 - b1)
            relatives[relative] += 1
            if relatives[relative] >= 12:
                return b2 - b1
    return None


def intersects(
    bs1: list[np.ndarray], bs2: list[np.ndarray]
) -> Optional[tuple[np.ndarray, np.ndarray]]:
    """
    The intersection between two scanners.

    :param bs1 list[np.ndarray]: The first scanner, considered origo.
    :param bs2 list[np.ndarray]: The second scanner, to rotate and transform.
    :rtype Optional[tuple[np.ndarray, np.ndarray]]: Rotation and transformation of
                                                    intersection.
    """
    for matrix in matrices:
        rbs2 = []
        for b in bs2:
            rbs2.append(matmul(matrix, np.array(b)))

        rel = r_intersects(bs1, rbs2)
        if rel is not None:
            return matrix, rel
    return None


# Store which we have checked against to avoid double checking because it is pretty slow
checked_against: dict[int, set[int]] = defaultdict(set)

fixeds: set[int] = {0}
positions = [np.array([0, 0, 0])]
while len(fixeds) < len(scanners):
    for i, beacons in enumerate(scanners):
        if i in fixeds:
            continue

        intersection = None
        for fixed in fixeds:
            if fixed in checked_against[i]:
                continue

            checked_against[i].add(fixed)

            intersection = intersects(scanners[fixed], beacons)
            if intersection is not None:
                break

        if intersection is not None:
            matrix, relative = intersection
            scanners[i] = [matmul(matrix, v) - relative for v in beacons]
            positions.append(relative)

            fixeds.add(i)

all_beacons = set()
for beacons in scanners:
    for beacon in beacons:
        all_beacons.add(tuple(beacon))
silver = len(all_beacons)


def manhattan_distance(v1, v2):
    return abs(v1[0] - v2[0]) + abs(v1[1] - v2[1]) + abs(v1[2] - v2[2])


gold = max(
    chain.from_iterable(
        [
            manhattan_distance(p1, p2)
            for j, p2 in enumerate(positions)
            if j != i
        ]
        for i, p1 in enumerate(positions)
    )
)


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
