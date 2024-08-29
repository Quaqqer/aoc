from collections import deque
from typing import cast

from aocd.models import Puzzle
from util import Grid, Heap, Vec2

puzzle = Puzzle(2019, int("18"))
data = puzzle.input_data


def find_distances(g: Grid[str], start: Vec2, exhaustive=False) -> dict[str, int]:
    visited = {start}

    distances: dict[str, int] = {}

    q = deque([(0, start)])

    while q:
        cost, pos = q.popleft()

        for neigh in (pos + delta for delta in Vec2.delta_4()):
            if g[neigh] == "#":
                continue

            if neigh not in visited:
                if g[neigh] != ".":
                    distances[g[neigh]] = cost + 1

                    if not exhaustive:
                        continue

                visited.add(neigh)
                q.append((cost + 1, neigh))

    return distances


def search_a(g: Grid[str]) -> int:
    [start] = map(Vec2, g.find_value("@"))

    keys = set(map(Vec2, g.find(lambda _, v: v.islower())))
    doors = set(map(Vec2, g.find(lambda _, v: v.isupper())))
    n_keys = len(keys)

    distances = {g[from_]: find_distances(g, from_) for from_ in keys | doors | {start}}

    q = Heap([(0, "@", frozenset())])
    visited: set[tuple[str, frozenset[str]]] = set()
    while q:
        cost, pos, keys = q.pop()

        state = pos, keys
        if state in visited:
            continue
        visited.add(state)

        if len(keys) == n_keys:
            return cost

        for neigh, neigh_cost in distances[pos].items():
            if neigh.isupper() and neigh.lower() not in keys:
                continue

            new_cost = cost + neigh_cost
            new_pos = neigh
            new_keys = (keys | {new_pos}) if new_pos.islower() else keys
            state = new_pos, new_keys

            if state in visited:
                continue

            q.push((new_cost, new_pos, new_keys))
    raise Exception("Found no solution")


type Positions = tuple[str, str, str, str]


def search_b(g: Grid[str]) -> int:
    [original_start] = map(Vec2, g.find_value("@"))

    new_center = [
        ["0", "#", "1"],
        ["#", "#", "#"],
        ["2", "#", "3"],
    ]
    for dx in range(-1, 2):
        for dy in range(-1, 2):
            g[original_start + Vec2(dx, dy)] = new_center[dy + 1][dx + 1]

    keys = set(map(Vec2, g.find(lambda _, v: v.islower())))
    doors = set(map(Vec2, g.find(lambda _, v: v.isupper())))
    starts = set(map(Vec2, g.find(lambda _, v: v.isdigit())))
    n_keys = len(keys)

    distances = {g[from_]: find_distances(g, from_) for from_ in keys | doors | starts}

    distances_exhaustive = {
        g[from_]: find_distances(g, from_, exhaustive=True)
        for from_ in keys | doors | starts
    }

    start_positions = cast(Positions, tuple(g[p] for p in starts))

    q = Heap([(0, 0, start_positions, frozenset())])
    visited: set[tuple[Positions, frozenset[str]]] = set()

    def heuristic(positions: Positions, keys: frozenset[str]) -> int:
        s = 0

        for position in positions:
            s += max(
                *(
                    cost
                    for to, cost in distances_exhaustive[position].items()
                    if to.islower() and to not in keys
                ),
                0,
                0,
            )

        return s

    while q:
        _, cost, positions, keys = q.pop()

        state = positions, keys
        if state in visited:
            continue
        visited.add(state)

        if len(keys) == n_keys:
            return cost

        for i in range(4):
            pos = positions[i]
            for neigh, neigh_cost in distances[pos].items():
                if neigh.isupper() and neigh.lower() not in keys:
                    continue

                new_cost = cost + neigh_cost
                new_positions = cast(
                    Positions, positions[:i] + (neigh,) + positions[i + 1 :]
                )
                new_keys = (keys | {neigh}) if neigh.islower() else keys
                state = new_positions, keys
                new_heur = new_cost + heuristic(*state)

                if state in visited:
                    continue

                q.push((new_heur, new_cost, new_positions, new_keys))
    raise Exception("Found no solution")


g = Grid.from_lines(data)
puzzle.answer_a = search_a(g.copy())
puzzle.answer_b = search_b(g.copy())
