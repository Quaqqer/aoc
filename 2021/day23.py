#!/usr/bin/env python3
from collections import namedtuple
from copy import deepcopy
from heapq import heappop, heappush

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 23)
id = puzzle.input_data
lines = [[char for char in line] for line in id.split("\n")]


Amph = namedtuple("Amph", ["pos", "is_done"])
Pos = namedtuple("Pos", ["x", "y"])

costs = {
    "A": 1,
    "B": 10,
    "C": 100,
    "D": 1000,
}
cols = {
    "A": 3,
    "B": 5,
    "C": 7,
    "D": 9,
}
hallway_moves = list(map(lambda x: Pos(x=x, y=1), [1, 2, 4, 6, 8, 10, 11]))


class State:
    def __init__(self, map, depth=2):
        self.map = map
        self.depth = depth
        self.filled = {"A": 0, "B": 0, "C": 0, "D": 0}
        self.cost = 0
        amphs = {}
        for char in {"A", "B", "C", "D"}:
            amphs[char] = self.find(char)
        self.amphs: dict[str, list[Amph]] = amphs
        self.update_heuristic()

    def frozen_map(self):
        return tuple(map(tuple, self.map))

    def find(self, char):
        chars = []
        for row in range(len(self.map)):
            for col in range(len(self.map[row])):
                if self.map[row][col] == char:
                    pos = Pos(x=col, y=row)
                    amph = Amph(pos=pos, is_done=False)
                    chars.append(amph)
        return chars

    def __lt__(self, other):
        return self.cost + self.h < other.cost + other.h

    def md(self, p1, p2):
        return abs(p1.x - p2.x) + abs(p1.y - p2.y)

    def __repr__(self):
        return "\n".join("".join(row) for row in self.map)

    def home_empty(self, char) -> bool:
        return all(self.map[i][cols[char]] == "." for i in range(2, 2 + self.depth))

    def path_free(self, f, t) -> bool:
        if f.y == 1:
            for x in range(min(f.x, t.x), max(f.x, t.x) + 1):
                if self.map[f.y][x] != "." and x != f.x:
                    return False
            for y in range(f.y, t.y + 1):
                if self.map[y][t.x] != ".":
                    return False
        else:
            for y in range(t.y, f.y):
                if self.map[y][f.x] != ".":
                    return False
            for x in range(min(t.x, f.x), max(t.x, f.x) + 1):
                if self.map[t.y][x] != ".":
                    return False
        return True

    def set(self, pos, v):
        self.map[pos.y][pos.x] = v

    def get(self, pos):
        return self.map[pos.y][pos.x]

    def move(self, char, chari, to, final: bool):
        amph = self.amphs[char][chari]
        self.cost += self.md(amph.pos, to) * costs[char]
        self.set(to, self.get(amph.pos))
        self.set(amph.pos, ".")
        self.amphs[char][chari] = Amph(pos=to, is_done=final)
        self.update_heuristic()

    def is_finished(self) -> bool:
        return all(map(lambda v: v == self.depth, self.filled.values()))

    def update_heuristic(self):
        h = 0
        for char in {"A", "B", "C", "D"}:
            for amph in self.amphs[char]:
                if not amph.is_done:
                    h += costs[char] * (
                        (
                            abs(amph.pos.x - cols[char])
                            if amph.pos.x != cols[char]
                            else 2
                        )
                        + 1
                        + abs(amph.pos.y - 1)
                    )
        self.h = h

    def children(self) -> list:
        children = []
        # from wrong home to hallway
        for char in {"A", "B", "C", "D"}:
            for chari, amph in enumerate(self.amphs[char]):
                for hp in hallway_moves:
                    if (
                        not amph.is_done
                        and amph.pos.y != 1
                        and self.path_free(amph.pos, hp)
                    ):
                        child = deepcopy(self)
                        child.move(char, chari, hp, False)
                        children.append(child)

        # from hallway to home
        for char in {"A", "B", "C", "D"}:
            for chari, amph in enumerate(self.amphs[char]):
                if (
                    not amph.is_done
                    and amph.pos.y == 1
                    and (self.home_empty(char) or self.filled[char] > 0)
                ):
                    newpos = Pos(y=1 + self.depth - self.filled[char], x=cols[char])
                    if self.path_free(amph.pos, newpos):
                        child = deepcopy(self)
                        child.move(char, chari, newpos, True)
                        child.filled[char] += 1
                        children.append(child)
        return children


def astar(initial):
    q = [initial]
    milestone = 1000

    visited = set()

    while len(q) > 0:
        s = heappop(q)

        fm = s.frozen_map()

        if fm in visited:
            continue

        visited.add(fm)

        if s.cost >= milestone:
            print(f"Cost {milestone} reached!")
            print(s)
            milestone += 1000

        if s.is_finished():
            print("FINISHED:")
            print(s)
            return s.cost

        for child in s.children():
            if child.frozen_map() not in visited:
                heappush(q, child)
    else:
        raise Exception("Found no solution.")


initial = State(lines)
# silver = astar(initial)

gold_d_lines = [[c for c in line] for line in """  #D#C#B#A#
  #D#B#A#C#""".splitlines()]
gold_map = lines[:3] + gold_d_lines + lines[3:]
gold_initial = State(gold_map, depth=4)

gold = astar(gold_initial)


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
