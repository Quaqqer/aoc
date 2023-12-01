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
