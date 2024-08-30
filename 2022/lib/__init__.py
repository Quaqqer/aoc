from __future__ import annotations

import re
from typing import Generator as _Generator
from typing import TypeVar as _TypeVar

from . import grid

_T = _TypeVar("_T")


def tup_add(t1, t2):
    return tuple(e1 + e2 for e1, e2 in zip(t1, t2))


def tup_sub(t1, t2):
    return tuple(e1 - e2 for e1, e2 in zip(t1, t2))


def tup_mul(t1, t2):
    return tuple(e1 * e2 for e1, e2 in zip(t1, t2))


def tup_div(t1, t2):
    return tuple(e1 / e2 for e1, e2 in zip(t1, t2))


def windows(li: list[_T], size: int) -> _Generator[list[_T], None, None]:
    for i in range(len(li) - size):
        yield li[i : i + size]


def sign(x: int) -> int:
    if x > 0:
        return 1
    elif x < 0:
        return -1
    else:
        return 0


def unzip(x):
    return zip(*x)


def all_ints(s: str) -> list[int]:
    return list(map(int, re.findall(r"-?\d+", s)))


def all_floats(s: str) -> list[float]:
    """Expects format of 0.0, not .0 or 0."""
    return list(map(float, re.findall(r"-?\d+\.\d+", s)))
