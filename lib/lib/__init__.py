from __future__ import annotations

from typing import Generator as _Generator
from typing import TypeVar as _TypeVar

_T = _TypeVar("_T")


def tup_add(t1, t2):
    return tuple(e1 + e2 for e1, e2 in zip(t1, t2))


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
