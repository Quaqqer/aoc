#!/usr/bin/env python3
from collections import defaultdict

from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 12)
lines = puzzle.input_data.split("\n")


# Main code
connections = defaultdict(list)
for line in lines:
    a, b = tuple(line.split("-"))
    connections[a].append(b)
    connections[b].append(a)


paths = []
visited = set()


def dfs(path, connections, visited, paths):
    pos = path[-1]
    if pos == "end":
        paths.append(path.copy())
    else:
        for c in connections[pos]:
            c: str
            if c.isupper():
                path.append(c)
                dfs(path, connections, visited, paths)
            elif c not in visited:
                visited.add(c)
                path.append(c)
                dfs(path, connections, visited, paths)
                visited.remove(c)
    path.pop()


dfs(["start"], connections, {"start"}, paths)
silver = len(paths)

paths = []


def dfs(path, connections, visited, paths, double_visited):
    pos = path[-1]
    if pos == "end":
        paths.append(path.copy())
    else:
        for c in connections[pos]:
            c: str
            if c.isupper():
                path.append(c)
                dfs(path, connections, visited, paths, double_visited)
            elif visited[c] == 0:
                visited[c] += 1
                path.append(c)
                dfs(path, connections, visited, paths, double_visited)
                visited[c] -= 1
            elif visited[c] == 1 and not double_visited:
                visited[c] += 1
                path.append(c)
                dfs(path, connections, visited, paths, True)
                visited[c] -= 1
    path.pop()


visited = defaultdict(int)
visited["start"] = 2
dfs(["start"], connections, visited, paths, False)
gold = len(paths)

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
