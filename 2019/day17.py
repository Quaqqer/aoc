from collections import defaultdict

import util
from aocd.models import Puzzle
from intpc import IntPC
from util import Grid, Vec2

puzzle = Puzzle(2019, int("17"))
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


def render(outputs: list[int]) -> Grid[str]:
    screen: list[str] = []
    screen = [chr(code) for code in outputs]
    screen.pop()
    screen.pop()

    return Grid.from_lines("".join(screen))


intpc = IntPC(program)
intpc.run()
g = render(intpc.outputs)


puzzle.answer_a = sum(
    x * y
    for x, y in g.find(
        lambda pos, _: g[pos] == "#" and all(g[n] == "#" for n in g.neighbours4(pos))
    )
)


[start] = g.find_value("^")
delta = Vec2(0, -1)
pos = Vec2.from_tuple(start)

path: tuple[str, ...] = ()

assert g[(pos + delta).to_tuple()] != "#"

while True:
    if (right := g.get(*(pos + delta.rot_r()).to_tuple())) == "#":
        turn = "R"
        delta = delta.rot_r()
    elif (left := g.get(*(pos + delta.rot_l()).to_tuple())) == "#":
        turn = "L"
        delta = delta.rot_l()
    else:
        break

    moved = 0
    while (next := g.get(*(pos + delta).to_tuple())) == "#":
        moved += 1
        pos += delta

    path += (f"{turn},{moved}",)


def find_abc(
    path: tuple[str, ...],
    abc: tuple[tuple[str, ...], ...] = (),
    history: tuple[str, ...] = (),
) -> tuple[tuple[str, ...], str] | None:
    if len(",".join(history)) > 20:
        return None

    if path == ():
        return tuple(",".join(s) for s in abc), ",".join(history)

    for i, part in enumerate(abc):
        if path[: len(part)] == part:
            ans = find_abc(path[len(part) :], abc, history + (chr(ord("A") + i),))

            if ans is not None:
                return ans

    if len(abc) < 3:
        for i in range(1, len(path)):
            part = path[:i]

            if len(",".join(part)) > 20:
                break

            ans = find_abc(path, abc + (part,), history)

            if ans is not None:
                return ans


ans = find_abc(path)
assert ans is not None
(a, b, c), main = ans


intpc = IntPC(program)
intpc.mem[0] = 2

for p in [main, a, b, c]:
    intpc.queue_input(f"{p}\n")

intpc.queue_input("n\n")

puzzle.answer_b = intpc.io([])[-1]
