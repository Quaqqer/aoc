# 9   00:14:17   997      0   01:12:06  3305      0
# Pretty hard day, easy to get wrong

from typing import Literal

from aocd.models import Puzzle

puzzle = Puzzle(2024, int("09"))

data = puzzle.input_data
data = "2333133121414131402"


def parse() -> list[int | Literal["."]]:
    fs = []

    for i in range(len(data)):
        c = int(data[i])
        if i % 2 == 0:
            fs += [i // 2] * c
        else:
            fs += ["."] * c

    return fs


def hash(fs: list[int | Literal["."]]) -> int:
    s = 0
    for i, v in enumerate(fs):
        if v != ".":
            s += i * int(v)
    return s


def solve_a():
    fs = parse()

    l = 0
    r = len(fs) - 1
    while l < r:
        if fs[l] != ".":
            l += 1
        elif fs[r] == ".":
            r -= 1
        else:
            fs[l], fs[r] = fs[r], fs[l]

    return hash(fs)


def solve_b():
    fs = parse()

    r = len(fs) - 1
    while r > 0:
        # Skip empty spaces
        if fs[r] == ".":
            r -= 1
            continue

        # Find the size of the right chunk
        rsize = 1
        while fs[r - 1] == fs[r]:
            rsize += 1
            r -= 1

        # Find a left chunk to swap
        l = 0
        found = False
        while l < r and not found:
            if fs[l] == ".":
                lsize = 1
                while fs[l + lsize] == ".":
                    lsize += 1
                if rsize <= lsize:
                    found = True
                    break
                else:
                    l += lsize
            else:
                l += 1

        if found:
            for i in range(rsize):
                fs[l + i] = fs[r + i]
                fs[r + i] = "."
            r -= rsize
        else:
            r -= 1

    return hash(fs)


a = solve_a()
print(a)
puzzle.answer_a = a
b = solve_b()
print(b)
puzzle.answer_b = b
