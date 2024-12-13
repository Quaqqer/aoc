# 9   00:14:17   997      0   01:12:06  3305      0
# Pretty hard day, very easy to get one off errors.

from typing import Literal

from aocd.models import Puzzle

puzzle = Puzzle(2024, int("09"))

data = puzzle.input_data


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

        # Find the size of the right chunk, and decrement r to the start of it
        rsize = 1
        while fs[r - 1] == fs[r]:
            rsize += 1
            r -= 1

        # Find a left chunk to swap
        l = 0
        while l < r:
            # If it's not empty, skip
            if fs[l] != ".":
                l += 1
                continue

            # Increment while we find more dots
            lsize = 1
            while fs[l + lsize] == ".":
                lsize += 1

            # If we find space, move it
            # Check if we found it
            if rsize <= lsize:
                for i in range(rsize):
                    fs[l + i] = fs[r + i]
                    fs[r + i] = "."
                break
            else:
                l += lsize

        r -= 1

    print("".join(map(str, fs)))

    return hash(fs)


puzzle.answer_a = solve_a()
puzzle.answer_b = solve_b()
