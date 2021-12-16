#!/usr/bin/env python3
from aocd.models import Puzzle
from operator import mul
from functools import reduce

# Parse input
puzzle = Puzzle(2021, 16)
input = [c for c in puzzle.input_data]


# Main code
conversions = {
    "0": "0000",
    "1": "0001",
    "2": "0010",
    "3": "0011",
    "4": "0100",
    "5": "0101",
    "6": "0110",
    "7": "0111",
    "8": "1000",
    "9": "1001",
    "A": "1010",
    "B": "1011",
    "C": "1100",
    "D": "1101",
    "E": "1110",
    "F": "1111",
}


def get(input: list[str]) -> list[str]:
    return [c for c in conversions[input.pop(0)]]


def consume(input: list[str], buf: list[str], n: int, consumed: int) -> tuple[list[str], str, int]:
    while len(buf) < n:
        buf += get(input)

    val = buf[:n]
    return buf[n:], "".join(val), consumed + n


def bint(bin: str) -> int:
    return int(bin, 2)


silver = 0


def read(buf: list[str], input: list[str]) -> tuple[list[str], str, int]:
    consumed = 0
    buf, _ver, consumed = consume(input, buf, 3, consumed)
    ver = bint(_ver)

    global silver
    silver += ver

    buf, _ty, consumed = consume(input, buf, 3, consumed)
    ty = bint(_ty)

    if ty == 4:  # literal
        v = ""

        cont = "1"
        while cont == "1":
            buf, cont, consumed = consume(input, buf, 1, consumed)

            buf, _v, consumed = consume(input, buf, 4, consumed)
            v += _v

        return buf, v, consumed
    else:
        vals = []
        buf, length_id, consumed = consume(input, buf, 1, consumed)
        if length_id == "0":
            buf, _l, consumed = consume(input, buf, 15, consumed)
            length = bint(_l)
            subread = 0
            while subread < length:
                buf, _v, c = read(buf, input)
                subread += c
                vals.append(bint(_v))
            consumed += subread
        else:
            buf, _l, consumed = consume(input, buf, 11, consumed)
            length = bint(_l)

            for _ in range(length):
                buf, _v, subread = read(buf, input)
                vals.append(bint(_v))
                consumed += subread

        res: int = 0
        match ty:
            case 0: res = sum(vals)
            case 1: res = reduce(mul, vals)
            case 2: res = min(vals)
            case 3: res = max(vals)
            case 5: res = 1 if vals[0] > vals[1] else 0
            case 6: res = 1 if vals[0] < vals[1] else 0
            case 7: res = 1 if vals[0] == vals[1] else 0
            case _: raise ValueError
        return buf, bin(res)[2:], consumed


buf = []
buf, _v, _ = read(buf, input)
gold = bint(_v)


# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
