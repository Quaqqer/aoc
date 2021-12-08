#!/usr/bin/env python3
from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2021, 8)
lines = puzzle.input_data.split("\n")

n_chars_num = {2: [1], 3: [7], 4: [4], 5: [2, 3, 5], 6: [6, 9, 0], 7: [8]}

segments = {
    0: {"a", "b", "c", "e", "f", "g"},
    1: {"c", "f"},
    2: {"a", "c", "d", "e", "g"},
    3: {"a", "c", "d", "f", "g"},
    4: {"b", "c", "d", "f"},
    5: {"a", "b", "d", "f", "g"},
    6: {"a", "b", "d", "e", "f", "g"},
    7: {"a", "c", "f"},
    8: {"a", "b", "c", "d", "e", "f", "g"},
    9: {"a", "b", "c", "d", "f", "g"},
}


# Main code
cnt = 0
for line in lines:
    input, output = tuple(line.split("|"))
    output_words = output.strip().split(" ")
    for w in output_words:
        if len(n_chars_num[len(w)]) == 1:
            cnt += 1
silver = cnt

vals = []
for line in lines:
    input, output = tuple(line.split("|"))
    input_words = input.strip().split(" ")
    output_words = output.strip().split(" ")

    segmap = {}

    eight = list(filter(lambda w: len(w) == 7, input_words))[0]
    segmap[8] = set(eight)

    one = list(filter(lambda w: len(w) == 2, input_words))[0]
    segmap[1] = set(one)

    seven = list(filter(lambda w: len(w) == 3, input_words))[0]
    segmap[7] = set(seven)

    four = list(filter(lambda w: len(w) == 4, input_words))[0]
    segmap[4] = set(four)

    # two three five
    ttfs = list(filter(lambda w: len(w) == 5, input_words))
    for ttf in ttfs:
        if len(set(ttf).intersection(segmap[1])) == 2:  # its a three
            segmap[3] = set(ttf)
        elif len(set(ttf).intersection(segmap[4])) == 3:  # its a five
            segmap[5] = set(ttf)
        else:
            segmap[2] = set(ttf)  # its a two

    snzs = list(filter(lambda w: len(w) == 6, input_words))
    for snz in snzs:
        if len(set(snz).intersection(segmap[5])) == 4:  # its a zero
            segmap[0] = set(snz)
        elif len(set(snz).intersection(segmap[1])) == 2:  # its a nine
            segmap[9] = set(snz)
        else:  # its a six
            segmap[6] = set(snz)

    print(segmap)

    val = 0
    for ow in output_words:
        number = [n for (n, m) in segmap.items() if m == set(ow)][0]
        val = val * 10 + number

    vals.append(val)

gold = sum(vals)


# gold = sum(vals)

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
