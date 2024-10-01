import json

from aocd.models import Puzzle

puzzle = Puzzle(2015, int("12"))
data = puzzle.input_data
lines = data.splitlines()

type Json = int | list[Json] | dict[str, Json] | str | bool | None


def count_numbers(data: Json, skip_red=False) -> int:
    match data:
        case list(l):
            return sum(count_numbers(e, skip_red) for e in l)
        case dict(d):
            return (
                0
                if skip_red and "red" in d.values()
                else sum(count_numbers(e, skip_red) for e in d.values())
            )
        case int(n):
            return n
        case str(_) | bool(_) | None:
            return 0


puzzle.answer_a = count_numbers(json.loads(data))
puzzle.answer_b = count_numbers(json.loads(data), skip_red=True)
