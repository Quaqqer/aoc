#!/usr/bin/env python3
import functools
import itertools

from aocd.models import Puzzle

puzzle = Puzzle(2021, 21)
lines = puzzle.input_data.split("\n")

p1_pos = int(lines[0].split(" ")[4])
p2_pos = int(lines[1].split(" ")[4])


def part_a(p1_pos, p2_pos):
    die = [1]
    rolls = [0]

    def roll_die() -> int:
        v = die[0]

        rolls[0] += 1
        die[0] += 1
        if die[0] > 100:
            die[0] = 1

        return v

    def triroll():
        return sum(roll_die() for _ in range(3))

    def step(pos, score):
        roll = triroll()
        pos = ((pos + roll - 1) % 10) + 1
        score += pos
        return pos, score

    p1_score, p2_score = 0, 0
    turn = True
    while max(p1_score, p2_score) < 1000:
        if turn:
            p1_pos, p1_score = step(p1_pos, p1_score)
        else:
            p2_pos, p2_score = step(p2_pos, p2_score)
        turn = not turn

    return rolls[0] * min(p1_score, p2_score)


silver = part_a(p1_pos=2, p2_pos=1)


@functools.cache
def wins(p1_pos, p2_pos, p1_score=0, p2_score=0, turn=True) -> tuple[int, int]:
    if p1_score >= 21:
        return 1, 0
    if p2_score >= 21:
        return 0, 1

    p1ws, p2ws = 0, 0
    for rolls in itertools.product(range(1, 4), repeat=3):
        if turn:
            _p1_pos = ((p1_pos + sum(rolls) - 1) % 10) + 1
            _p1_score = p1_score + _p1_pos
            _p1ws, _p2ws = wins(_p1_pos, p2_pos, _p1_score, p2_score, not turn)
            p1ws, p2ws = p1ws + _p1ws, p2ws + _p2ws
        else:
            _p2_pos = ((p2_pos + sum(rolls) - 1) % 10) + 1
            _p2_score = p2_score + _p2_pos
            _p1ws, _p2ws = wins(p1_pos, _p2_pos, p1_score, _p2_score, not turn)
            p1ws += _p1ws
            p2ws += _p2ws

    return p1ws, p2ws


gold = max(wins(p1_pos, p2_pos))

# Print answers and send to aoc
if "silver" in locals():
    print(f"Silver: {silver}")  # type: ignore
    puzzle.answer_a = silver  # type: ignore
if "gold" in locals():
    print(f"Gold: {gold}")  # type: ignore
    puzzle.answer_b = gold  # type: ignore
