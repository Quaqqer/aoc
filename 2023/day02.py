# 2   00:12:00  1879      0   00:19:38  2760      0
# Reading comprehension...

from collections import defaultdict
from math import prod

from aocd.models import Puzzle

puzzle = Puzzle(2023, int("02"))
data = puzzle.input_data

lines = data.splitlines()

possible_sum = 0
min_prod = 0
for line in lines:
    game_name, game = line.split(": ")
    game_id = int(game_name.split(" ")[1])

    counts = defaultdict(int)

    for subgame in game.split("; "):
        for amt_col in subgame.split(", "):
            amt, col = amt_col.split(" ")
            counts[col] = max(counts[col], int(amt))

    if counts["red"] <= 12 and counts["green"] <= 13 and counts["blue"] <= 14:
        possible_sum += game_id

    min_prod += prod(counts.values())


puzzle.answer_a = possible_sum
puzzle.answer_b = min_prod
