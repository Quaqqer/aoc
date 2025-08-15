from aocd.models import Puzzle

puzzle = Puzzle(2017, int("06"))

banks = tuple(map(int, puzzle.input_data.split()))


def solve_a(banks: list[int]):
    seen: dict[tuple[int, ...], int] = {tuple(banks): 0}

    redistributions = 0
    while True:
        redistributions += 1

        selected_bank_v = max(banks)
        selected_bank_i = banks.index(selected_bank_v)

        banks[selected_bank_i] = 0
        pos = selected_bank_i
        for _ in range(selected_bank_v):
            pos = (pos + 1) % len(banks)
            banks[pos] += 1

        if tuple(banks) in seen:
            return redistributions, redistributions - seen[tuple(banks)]
        seen[tuple(banks)] = redistributions


puzzle.answer_a, puzzle.answer_b = solve_a(list(banks))
