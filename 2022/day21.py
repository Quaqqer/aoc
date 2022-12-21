# 21   01:13:21   4465      0   01:36:32   2873      0
# woke up at 7 today, so 1 hour late, but at least I won't have a headache from
# sleep deprivation :)

import re
from dataclasses import dataclass

from aocd.models import Puzzle

puzzle = Puzzle(2022, int("21"))
id = puzzle.input_data

Expression = int | str | tuple


def parse_line(line: str):
    monkey = line[:4]
    expr = line[6:]
    if m := re.match(r"-?\d+", expr):
        return (monkey, int(m.group(0)))
    else:
        return (monkey, tuple(expr.split()))


def create_expr(init_monkey: str, exprs: dict[str, Expression], p2: bool) -> Expression:
    def inner(monkey: str):
        if p2 and monkey == "humn":
            return "humn"

        match exprs[monkey]:
            case int(x):
                return x
            case (l, op, r):
                return inner(l), op, inner(r)
            case _:
                raise Exception()

    return inner(init_monkey)


def expreval(expr: Expression, float: bool = False) -> int:
    match expr:
        case int(x):
            return x
        case (l, op, r):
            e = eval(f"{expreval(l)} {op} {expreval(r)}")
            return e if float else int(e)
        case _:
            raise Exception()


def p2eval(init) -> Expression:
    def inner(expr):
        match expr:
            case str(x):
                return x
            case int(x):
                return x
            case (l, op, r):
                el = inner(l)
                er = inner(r)

                if type(el) != int or type(er) != int:
                    return (el, op, er)
                else:
                    return int(eval(f"{el} {op} {er}"))
            case _:
                raise Exception()

    return inner(init)


def p2solve(to_solve, ans) -> int:
    match to_solve:
        case "humn":
            return expreval(ans)

        case (l, "-", int(r)):
            return p2solve(l, (ans, "+", r))
        case (int(l), "-", r):
            return p2solve(r, (l, "-", ans))

        case (l, "/", int(r)):
            return p2solve(l, (ans, "*", r))
        case (int(l), "/", r):
            return p2solve(r, (l, "/", ans))

        case (int(l), "+", r):
            return p2solve(r, (ans, "-", l))
        case (l, "+", int(r)):
            return p2solve(l, (ans, "-", r))

        case (int(l), "*", r):
            return p2solve(r, (ans, "/", l))
        case (l, "*", int(r)):
            return p2solve(l, (ans, "/", r))

        case _:
            raise Exception(f"{type(to_solve[0])} {to_solve[1]} {type(to_solve[2])}")


exprs: dict[str, Expression] = dict(map(parse_line, id.splitlines()))
puzzle.answer_a = expreval(create_expr("root", exprs, False))


root_expr = create_expr("root", exprs, True)
assert isinstance(root_expr, tuple)
lhs, _, rhs = root_expr
# Assume that lhs is the part to solve, might be wrong but I don't care to fix it.
puzzle.answer_b = p2solve(p2eval(lhs), p2eval(rhs))
