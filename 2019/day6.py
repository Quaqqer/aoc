from collections import defaultdict

from aocd.models import Puzzle

puzzle = Puzzle(2019, int("06"))
data = puzzle.input_data
lines = data.splitlines()

orbits = defaultdict(list)

for line in lines:
    a, b = line.split(")")
    orbits[a].append(b)


def find_orbits(p: str) -> int:
    v = 0
    for p2 in orbits[p]:
        v += 1
        if p2 in orbits:
            v += find_orbits(p2)
    return v


s = 0
for p1 in orbits:
    s += find_orbits(p1)
puzzle.answer_a = s


orbits_undirected = defaultdict(list)
for k, vs in orbits.items():
    orbits_undirected[k] += vs
    for v in vs:
        orbits_undirected[v].append(k)


def find_dist(n, end) -> int:
    visited = set()

    def dfs(v) -> int | None:
        if v == end:
            return 0

        for vv in orbits_undirected[v]:
            if vv not in visited:
                visited.add(vv)
                d = dfs(vv)
                if d is not None:
                    return d + 1

    ans = dfs(n)
    assert ans is not None
    return ans


puzzle.answer_b = find_dist("YOU", "SAN") - 2
