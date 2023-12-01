#  1   00:03:23   936      0   00:12:18   639      0

from aocd.models import Puzzle

puzzle = Puzzle(2023, int("01"))
data = puzzle.input_data

names = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]


def solve_line(line: str, part_2: bool) -> int:
    def find_digit(r: range) -> str:
        for i in r:
            if line[i].isdigit():
                return line[i]
            elif part_2:
                for val, n in enumerate(names):
                    for j, c in enumerate(n):
                        if len(line) <= i + j or line[i + j] != c:
                            break
                    else:
                        return str(val)
        assert False

    l = find_digit(range(len(line)))
    r = find_digit(range(len(line))[::-1])
    return int(l + r)


puzzle.answer_a = sum(solve_line(l, False) for l in data.splitlines())
puzzle.answer_b = sum(solve_line(l, True) for l in data.splitlines())

# puzzle.answer_a = sum(int(''.join((ds := [c for c in line if c.isdigit()], ds[0], ds[-1])[1:])) for line in data.splitlines()) # noqa
# puzzle.answer_b = sum(int((ds := [v[0] for i in range(len(line)) if (v := [str(val % 10) for val, s in enumerate(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine']) if line[i:].startswith(s)]) and len(v) == 1], ds[0] + ds[-1])[1]) for line in data.splitlines()) # noqa
