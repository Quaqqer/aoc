from collections import defaultdict, deque

import util
from aocd.models import Puzzle
from intpc import IntPC
from util import Vec2

puzzle = Puzzle(2019, int("15"))
data = puzzle.input_data
program = util.ints(data.split(","))


dirs = [Vec2(0, -1), Vec2(0, 1), Vec2(-1, 0), Vec2(1, 0)]


def discover_map(pos: Vec2):
    opposite = [1, 0, 3, 2]

    map: dict[Vec2, str] = defaultdict(lambda: "?", {Vec2(0, 0): " "})

    intpc = IntPC(program)

    def move(dir: int) -> int:
        [output] = intpc.io([dir + 1])
        return output

    def inner(pos: Vec2):
        for i, dir in enumerate(dirs):
            if map[pos + dir] == "?":
                output = move(i)

                match output:
                    case 0:
                        map[pos + dir] = "#"
                    case 1:
                        map[pos + dir] = " "
                        inner(pos + dir)
                        move(opposite[i])
                    case 2:
                        map[pos + dir] = "O"
                        inner(pos + dir)
                        move(opposite[i])

    inner(pos)

    return map


def exhaustive_bfs(from_: Vec2) -> dict[Vec2, int]:
    q: deque[tuple[Vec2, int]] = deque([(from_, 0)])
    costs: dict[Vec2, int] = {from_: 0}

    while q:
        pos, i = q.popleft()

        for neigh in [pos + dir for dir in dirs]:
            if map[neigh] != "#" and neigh not in costs:
                q.append((neigh, i + 1))
                costs[neigh] = i + 1

    return costs


map = discover_map(Vec2(0, 0))
oxygen = list(map.keys())[list(map.values()).index("O")]


puzzle.answer_a = exhaustive_bfs(Vec2(0, 0))[oxygen]
puzzle.answer_b = max(exhaustive_bfs(oxygen).values())
