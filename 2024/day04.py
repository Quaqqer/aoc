# 4   00:07:12   990      0   00:17:36  1329      0
# It's alright, I guess
#
# Today I didnt' use puzzle.answer_a to submit but rather used a file watcher
# and printed the answers. It's probably a lot better to avoid submitting bad
# answers. You also get instant feedback if you are doing something wrong by
# getting an exception.

from collections import defaultdict

from aocd.models import Puzzle
from util import Grid, Vec2

puzzle = Puzzle(2024, int("04"))

g = Grid.from_lines(puzzle.input_data)


def solve_a():
    s = 0

    for x, y in g:
        for delta in Vec2.delta_8():
            for i, c in enumerate("XMAS"):
                n = Vec2(x, y) + delta * i

                if g.get(n.x, n.y, default=".") != c:
                    break
            else:
                s += 1

    return s


def solve_b():
    s = defaultdict(int)

    for x, y in g:
        s[x, y] = 0

        for delta in [Vec2(1, 1), Vec2(1, -1), Vec2(-1, 1), Vec2(-1, -1)]:
            for i, c in enumerate("MAS"):
                p = Vec2(x, y)
                n = p + delta * (i - 1)

                if g.get(n.x, n.y, default=".") != c:
                    break
            else:
                s[x, y] += 1

    return sum(v == 2 for v in s.values())


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
