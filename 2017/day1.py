#!/usr/bin/env python3
from aocd.models import Puzzle

# Parse input
puzzle = Puzzle(2017, 1)

# Main code
nums = [int(c) for c in puzzle.input_data]

puzzle.answer_a = sum([v for i, v in enumerate(nums) if nums[(i + 1) % len(nums)] == v])

puzzle.answer_b = sum(
    [v for i, v in enumerate(nums) if nums[(i + len(nums) // 2) % len(nums)] == v]
)
