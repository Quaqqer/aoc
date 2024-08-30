from collections import defaultdict, deque

from aocd.models import Puzzle
from util import Grid, Heap, Vec2

puzzle = Puzzle(2019, int("20"))
data = puzzle.input_data

g = Grid.from_lines(data)

portal_positions: dict[Vec2[int], str] = {}
for x in range(g.cols):
    for y in range(g.rows):
        pos = Vec2(x, y)

        c = g[pos]

        if not c.isupper():
            continue

        for delta in [Vec2(1, 0), Vec2(0, 1)]:
            pos2 = pos + delta
            if pos2 in g and g[pos2].isupper():
                l = g[pos] + g[pos2]

                if any(g[neigh] == "." for neigh in g.neighbours4(pos.tup)):
                    portal_positions[pos] = l
                    g[pos] = "o"
                else:
                    g[pos] = " "

                if any(g[neigh] == "." for neigh in g.neighbours4(pos2.tup)):
                    portal_positions[pos2] = l
                    g[pos2] = "o"
                else:
                    g[pos2] = " "


def is_outer(pos: Vec2[int]) -> bool:
    return pos.x == 1 or pos.y == 1 or pos.x == g.cols - 2 or pos.y == g.rows - 2


def search_a() -> int:
    def find_distances(g: Grid[str], start: Vec2[int]) -> dict[str, int]:
        visited = {start}

        distances: dict[str, int] = {}

        q = deque([(0, start)])

        while q:
            cost, pos = q.popleft()

            for neigh in (pos + delta for delta in Vec2.delta_4()):
                if neigh not in visited and g[neigh] in {".", "o"}:
                    if g[neigh] == "o":
                        distances[portal_positions[neigh]] = cost

                        continue

                    visited.add(neigh)
                    q.append((cost + 1, neigh))

        return distances

    distances: dict[str, dict[str, int]] = defaultdict(dict)
    for pos, portal in portal_positions.items():
        dists = find_distances(g, pos)
        for dist, cost in dists.items():
            distances[portal][dist] = cost

    q = Heap([(-1, "AA")])

    visited = set()

    while q:
        cost, pos = q.pop()

        if pos in visited:
            continue
        else:
            visited.add(pos)

        if pos == "ZZ":
            return cost

        for portal, portal_cost in distances[pos].items():
            q.push((cost + portal_cost, portal))

    raise Exception("No solution found")


def search_b() -> int:
    def find_distances(start: Vec2[int]) -> dict[str, int]:
        visited = {start}

        distances: dict[str, int] = {}

        q = deque([(0, start)])

        while q:
            cost, pos = q.popleft()

            for neigh in (pos + delta for delta in Vec2.delta_4()):
                if neigh not in visited and g[neigh] in {".", "o"}:
                    if g[neigh] == "o":
                        outer = (
                            neigh.x == 1
                            or neigh.y == 1
                            or neigh.x == g.cols - 2
                            or neigh.y == g.rows - 2
                        )

                        distances[
                            portal_positions[neigh] + "_" + ("o" if outer else "i")
                        ] = cost

                        continue

                    visited.add(neigh)
                    q.append((cost + 1, neigh))

        return distances

    distances: dict[str, dict[str, int]] = {
        f'{portal}_{"o" if is_outer(pos) else "i"}': find_distances(pos)
        for pos, portal in portal_positions.items()
    }

    q = Heap([(-1, 0, "AA_o")])

    visited = set()

    def flip_portal(p: str) -> str:
        if p.endswith("o"):
            return p[:-1] + "i"
        else:
            return p[:-1] + "o"

    while q:
        cost, layer, pos = q.pop()

        if pos in visited:
            continue
        else:
            visited.add((pos, layer))

        if pos == "ZZ_i":
            return cost

        for portal, portal_cost in distances[pos].items():
            if layer == 0:
                if portal.endswith("o") and portal != "ZZ_o":
                    continue
            elif layer != 0:
                if portal in {"AA_o", "ZZ_o"}:
                    continue

            new_layer = layer + (1 if portal.endswith("i") else -1)

            q.push((cost + portal_cost, new_layer, flip_portal(portal)))

    raise Exception("No solution found")


puzzle.answer_a = search_a()
puzzle.answer_b = search_b()
