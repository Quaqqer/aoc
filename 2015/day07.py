from collections import deque

from aocd.models import Puzzle

puzzle = Puzzle(2015, int("7"))
data = puzzle.input_data
lines = data.splitlines()


def eval_expr(values: dict[str, int], expr: str) -> int | None:
    """A very cursed function to evaluate an expression"""

    def inner() -> int:
        def get(v: str) -> int:
            try:
                return int(v)
            except ValueError:
                pass
            return values[v]

        match expr.split():
            case [a]:
                return get(a)
            case a, "AND", b:
                return get(a) & get(b)
            case a, "OR", b:
                return get(a) | get(b)
            case "NOT", a:
                return 0xFFFF - get(a)
            case a, "RSHIFT", b:
                return get(a) >> get(b)
            case a, "LSHIFT", b:
                return get(a) << get(b)
            case u:
                raise Exception(f"Unhandled op: {u}")

    # If a key error is raised we can't evaluate, return None
    try:
        return inner()
    except KeyError:
        pass


def find_signal_a(exprs: dict[str, str]):
    q = deque(exprs)
    values = {}

    while q:
        signal = q.popleft()

        res = eval_expr(values, exprs[signal])

        if res is not None:
            values[signal] = res

            if signal == "a":
                return res
        else:
            q.append(signal)

    raise Exception("No solution")


exprs = {}

for line in lines:
    *expr, _, res = line.split()
    exprs[res] = " ".join(expr)


puzzle.answer_a = find_signal_a(exprs)
puzzle.answer_b = find_signal_a(exprs | {"b": puzzle.answer_a})
