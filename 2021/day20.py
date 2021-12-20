#!/usr/bin/env python3
from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 20)
algorithm, image_str = puzzle.input_data.split("\n\n")

assert len(algorithm) == 512

image_lines = image_str.splitlines()
image = set()
for y in range(len(image_lines)):
    for x in range(len(image_lines[0])):
        if image_lines[y][x] == "#":
            image.add((x, y))


# Main code
def _neighbins(image: set[tuple[int, int]], pos: tuple[int, int]) -> int:
    bin = ""
    x, y = pos
    for dy in range(-1, 1 + 1):
        for dx in range(-1, 1 + 1):
            np = x + dx, y + dy
            bin += "1" if np in image else "0"
    return int(bin, 2)


def apply(image: set[tuple[int, int]], algorithm: str) -> set[tuple[int, int]]:
    visited = set()
    new_image = set()

    def update_neighbours(pos: tuple[int, int]):
        x, y = pos
        for dy in range(-1, 1 + 1):
            for dx in range(-1, 1 + 1):
                np = x + dx, y + dy
                if np not in visited:
                    visited.add(np)
                    if algorithm[_neighbins(image, np)] == "#":
                        new_image.add(np)

    for x, y in image:
        update_neighbours((x, y))

    return new_image


def draw(image: set[tuple[int, int]]):
    xs = list(map(lambda v: v[0], image))
    ys = list(map(lambda v: v[1], image))

    min_x, max_x = min(xs), max(xs)
    min_y, max_y = min(ys), max(ys)

    for y in range(min_y - 1, max_y + 2):
        line = []
        for x in range(min_x - 1, max_x + 2):
            line.append("#" if (x, y) in image else ".")
        print("".join(line))


print("Draw 1")
draw(image)
for i in range(2):
    image = apply(image, algorithm)
    print(f"Draw {i + 2}")
    draw(image)
silver = len(image)


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
