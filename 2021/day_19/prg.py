#!/usr/bin/env python3
from collections import defaultdict
from itertools import permutations
from typing import Optional

import numpy as np
from aocd.models import Puzzle
from numpy import matmul

puzzle = Puzzle(2021, 19)
scanners_input = puzzle.input_data.split("\n\n")

scanners: list[list[np.ndarray]] = []

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


for i in range(len(scanners_input)):
    beacons = scanners_input[i].splitlines()[1:]
    scanner = []
    for beacon in beacons:
        b = np.array([int(v) for v in beacon.split(",")])
        scanner.append(b)
    scanners.append(scanner)


def _intersects(bs1: list[np.ndarray], bs2: list[np.ndarray]) -> Optional[np.ndarray]:
    # relative counter
    relatives = defaultdict(int)
    for b1 in bs1:
        for b2 in bs2:
            relative = tuple(b1 - b2)
            relatives[relative] += 1
            if relatives[relative] >= 12:
                return b1 - b2
    return None


def intersects(
    bs1: list[np.ndarray], bs2: list[np.ndarray]
) -> Optional[tuple[np.ndarray, np.ndarray]]:
    for matrix in matrices:
        rbs2 = []
        for b in bs2:
            rbs2.append(matmul(matrix, np.array(b)))

        rel = _intersects(bs1, rbs2)
        if rel is not None:
            return matrix, rel
    return None


fixeds = {0}
positions = [np.array([0, 0, 0])]
while len(fixeds) < len(scanners):
    for i in range(len(scanners)):
        if i not in fixeds:
            intersection = None
            for fixed in fixeds:
                intersection = intersects(scanners[fixed], scanners[i])
                if intersection is not None:
                    break
            if intersection is not None:
                matrix, relative = intersection
                scanners[i] = [matmul(matrix, v) + relative for v in scanners[i]]
                positions.append(relative)
                fixeds.add(i)

all_beacons = set()
for beacons in scanners:
    for beacon in beacons:
        all_beacons.add(tuple(beacon))
silver = len(all_beacons)


def manhattan_distance(v1, v2):
    return abs(v1[0] - v2[0]) + abs(v1[1] - v2[1]) + abs(v1[2] - v2[2])


distances = []
for i in range(len(positions)):
    for j in range(len(positions)):
        if i == j:
            continue
        distances.append(manhattan_distance(positions[i], positions[j]))
gold = max(distances)


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
