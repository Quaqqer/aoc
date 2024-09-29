import functools
import itertools
import math
import re
from collections import Counter, defaultdict, deque
from copy import copy, deepcopy
from dataclasses import dataclass

import util
from aocd.models import Puzzle
from util import Vec2, Vec3, ints

puzzle = Puzzle(2015, int("11"))
data = puzzle.input_data
lines = data.splitlines()


def next_ch(c: str) -> tuple[str, bool]:
    if c == "z":
        return "a", True
    return chr(ord(c) + 1), False


def valid(password: str) -> bool:
    triple_increasing = any(
        ord(a) + 1 == ord(b) and ord(b) + 1 == ord(c)
        for a, b, c in zip(password, password[1:], password[2:])
    )
    no_iol = not any(c in ("i", "o", "l") for c in password)
    pairs = 0
    i = 0
    while i < len(password) - 1:
        if password[i] == password[i + 1]:
            i += 2
            pairs += 1
        else:
            i += 1

    return pairs >= 2 and triple_increasing and no_iol


def next_str(s: str) -> str:
    chrs = list(s)
    for i in range(len(s))[::-1]:
        c, wrapped = next_ch(chrs[i])
        chrs[i] = c

        if not wrapped:
            break

    return "".join(chrs)


def next_password(password: str) -> str:
    password = next_str(password)
    while not valid(password):
        password = next_str(password)
    return password


puzzle.answer_a = next_password(data)
puzzle.answer_b = next_password(puzzle.answer_a)
