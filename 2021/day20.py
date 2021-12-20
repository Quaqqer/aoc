#!/usr/bin/env python3
from aocd.models import Puzzle

"""
WARNING: Ugly hardcode, assuming that infinite grid toggles between on and off in input.
"""

# Parse input
puzzle = Puzzle(2021, 20)
algorithm, image_str = puzzle.input_data.split("\n\n")

assert len(algorithm) == 512

image_lines = image_str.splitlines()
image = dict()
for y in range(len(image_lines)):
    for x in range(len(image_lines[0])):
        image[(x, y)] = image_lines[y][x] == "#"
inf = False


# Main code
def _neighbins(
    image: dict[tuple[int, int], bool], pos: tuple[int, int], inf: bool
) -> int:
    bin = ""
    x, y = pos
    for dy in range(-1, 1 + 1):
        for dx in range(-1, 1 + 1):
            np = x + dx, y + dy
            if np in image:
                bin += "1" if image[np] else "0"
            else:
                bin += "1" if inf else "0"
    return int(bin, 2)


def apply(
    image: dict[tuple[int, int], bool], inf: bool, algorithm: str
) -> tuple[dict[tuple[int, int], bool], bool]:
    visited = set()
    new_image = dict()

    def update_neighbours(pos: tuple[int, int]):
        x, y = pos
        for dy in range(-1, 1 + 1):
            for dx in range(-1, 1 + 1):
                np = x + dx, y + dy
                if np not in visited:
                    visited.add(np)
                    new_image[np] = algorithm[_neighbins(image, np, inf)] == "#"

    for x, y in image:
        update_neighbours((x, y))

    return new_image, not inf


def draw(image: dict[tuple[int, int], bool], inf):
    xs = list(map(lambda v: v[0], image.keys()))
    ys = list(map(lambda v: v[1], image.keys()))

    min_x, max_x = min(xs), max(xs)
    min_y, max_y = min(ys), max(ys)

    for y in range(min_y - 3, max_y + 4):
        line = []
        for x in range(min_x - 3, max_x + 4):
            if (x, y) in image:
                line.append("#" if image[(x, y)] else ".")
            else:
                line.append("#" if inf else ".")
        print("".join(line))


draw(image, inf)
for _ in range(2):
    image, inf = apply(image, inf, algorithm)
silver = sum(image.values())

for _ in range(50 - 2):
    image, inf = apply(image, inf, algorithm)
gold = sum(image.values())



# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
