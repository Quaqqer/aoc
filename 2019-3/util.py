from __future__ import annotations

import math
import re
from copy import deepcopy
from typing import (
    Any,
    Callable,
    Generator,
    Iterable,
    Sequence,
    overload,
)

type Coord = tuple[int, int]


class Grid[T]:
    def __init__(self, cols: int, rows: int, _grid: list[list[T]]):
        self._cols = cols
        self._rows = rows
        self._grid = _grid

    def copy(self) -> Grid[T]:
        return Grid(cols=self._cols, rows=self._rows, _grid=deepcopy(self._grid))

    def __repr__(self) -> str:
        is_strs = all(isinstance(self[x, y], str) for x, y in self.coords())
        return "\n".join(
            ("" if is_strs else " ").join(
                v if isinstance(v := self[x, y], str) else repr(v)
                for x in range(self.cols)
            )
            for y in range(self.rows)
        )

    @property
    def cols(self) -> int:
        return self._cols

    @property
    def rows(self) -> int:
        return self._rows

    def is_inside(self, x: int, y: int) -> bool:
        return 0 <= x < self._cols and 0 <= y < self._rows

    def is_outside(self, x: int, y: int) -> bool:
        return not self.is_inside(x, y)

    def __contains__(self, coord: Coord) -> bool:
        x, y = coord
        return self.is_inside(x, y)

    def get(self, x: int, y: int) -> T:
        """Get an item in the grid

        Args:
            x: The x position
            y: The y position

        Returns:
            The item
        """
        return self._grid[y][x]

    def set(self, x: int, y: int, v: T):
        """Set an item in the grid.

        Args:
            x: The x position
            y: The y position
            v: The value
        """
        self._grid[y][x] = v

    @overload
    def __getitem__(self, index: tuple[int, int]) -> T: ...

    @overload
    def __getitem__(self, index: tuple[slice, int] | tuple[int, slice]) -> list[T]: ...

    @overload
    def __getitem__(self, index: tuple[slice, slice]) -> Grid[T]: ...

    def __getitem__(
        self, index: tuple[int | slice, int | slice]
    ) -> T | list[T] | Grid[T]:
        """Get values in the grid

        Args:
            coord: (x, y) coordinate
        """
        x, y = index
        if isinstance(x, int) and isinstance(y, int):
            return self.get(x, y)
        elif isinstance(x, slice) and isinstance(y, int):
            return [self.get(xx, y) for xx in range(x.start, x.stop, x.step)]
        elif isinstance(x, int) and isinstance(y, slice):
            return [self.get(x, yy) for yy in range(y.start, y.stop, y.step)]
        else:
            assert isinstance(x, slice) and isinstance(y, slice)
            cols = x.stop - x.start
            rows = y.stop - y.start
            return Grid(
                cols,
                rows,
                [
                    [self.get(xx, yy) for xx in range(x.start, x.stop, x.step)]
                    for yy in range(y.start, y.stop, y.step)
                ],
            )

    def __setitem__(self, index: tuple[int | slice, int | slice], v: T) -> None:
        """Set an item in the grid

        Args:
            coord: (x, y) coordinate
            v: Value
        """
        x, y = index
        if isinstance(x, int) and isinstance(y, int):
            self.set(x, y, v)
        elif isinstance(x, slice) and isinstance(y, int):
            for xx in range(x.start, x.stop, x.step):
                self.set(xx, y, v)
        elif isinstance(x, int) and isinstance(y, slice):
            for yy in range(y.start, y.stop, y.step):
                self.set(x, yy, v)
        else:
            assert isinstance(x, slice) and isinstance(y, slice)
            for xx in range(x.start, x.stop, x.step):
                for yy in range(y.start, y.stop, y.step):
                    self.set(xx, yy, v)

    def map[U](self, fn: Callable[[T], U]) -> Grid[U]:
        return Grid(
            self.cols,
            self.rows,
            [[fn(self[x, y]) for x in range(self.cols)] for y in range(self.rows)],
        )

    def map_with_pos[U](self, fn: Callable[[tuple[int, int], T], U]) -> Grid[U]:
        return Grid(
            self.cols,
            self.rows,
            [
                [fn((x, y), self[x, y]) for x in range(self.cols)]
                for y in range(self.rows)
            ],
        )

    def transpose(self) -> Grid[T]:
        return Grid(
            self.rows,
            self.cols,
            [[self[y, x] for y in range(self.rows)] for x in range(self.cols)],
        )

    @staticmethod
    def from_lines(s: str) -> Grid[str]:
        lines = s.splitlines()

        assert len(lines) > 0
        rows = len(lines)
        cols = len(lines[0])
        assert all(cols == len(l) for l in lines)

        return Grid(cols, rows, [[c for c in l] for l in lines])

    @staticmethod
    def from_2d_list(l: list[list[T]]) -> Grid[T]:
        assert len(l) > 0
        rows = len(l)
        cols = len(l[0])
        assert all(cols == len(ll) for ll in l)

        return Grid(cols, rows, [[c for c in ll] for ll in l])

    def dfs(
        self,
        start: tuple[int, int],
        visit: Callable[[tuple[int, int], T], bool | None],
        is_visitable: Callable[[tuple[int, int], T], bool],
    ):
        """DFS Search.

        Args:
            start: The start coordinate
            visit: On visit function, return true to finish
            is_visitable: If a position is visitable

        Returns:
            The searched for position
        """
        visited = {start}

        def _inner(coord: tuple[int, int]) -> tuple[int, int] | None:
            visited.add(coord)

            end = visit(coord, self[coord])

            if end:
                return coord

            for n in self.neighbours4(coord):
                if n not in visited and is_visitable(coord, self[coord]):
                    res = _inner(coord)

                    if res is not None:
                        return res

        return _inner(start)

    def find(
        self, f: Callable[[tuple[int, int], T], bool]
    ) -> Generator[tuple[int, int], Any, None]:
        for x in range(self.cols):
            for y in range(self.rows):
                v = self[x, y]
                if f((x, y), v):
                    yield x, y

    def neighbours4(
        self, coord: tuple[int, int], inside: bool = True
    ) -> Generator[tuple[int, int], Any, None]:
        x, y = coord

        for dx, dy in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            if dx == dy == 0:
                continue

            nx = x + dx
            ny = y + dy

            if (not inside) or self.is_inside(nx, ny):
                yield nx, ny

    def neighbours8(
        self, coord: tuple[int, int], inside: bool = True
    ) -> Generator[tuple[int, int], Any, None]:
        x, y = coord

        for dx in [-1, 0, 1]:
            for dy in [-1, 0, 1]:
                if dx == dy == 0:
                    continue

                nx = x + dx
                ny = y + dy

                if (not inside) or self.is_inside(nx, ny):
                    yield nx, ny

    def coords(self) -> Generator[tuple[int, int], Any, None]:
        for x in range(self.cols):
            for y in range(self.rows):
                yield x, y

    def values(self) -> Generator[T, Any, None]:
        for x, y in self.coords():
            yield self[x, y]

    def __iter__(self):
        return self.coords()

    def __hash__(self):
        return hash(tuple(tuple(c for c in r) for r in self._grid))

    def __eq__(self, other: object):
        if not isinstance(other, Grid):
            return False

        if self.cols != other.cols or self.rows != other.rows:
            return False

        for pos in self:
            if self[pos] != other[pos]:
                return False

        return True


def tup_op[T](
    a: tuple[T, ...], b: tuple[T, ...], op: Callable[[T, T], T]
) -> tuple[T, ...]:
    return tuple(op(aa, bb) for aa, bb in zip(a, b))


def tup_add[*Ts](a: tuple[*Ts], b: tuple[*Ts]) -> tuple[*Ts]:
    return tup_op(a, b, lambda aa, bb: aa + bb)


def tup_sub[T](a: tuple[T, ...], b: tuple[T, ...]) -> tuple[T, ...]:
    return tup_op(a, b, lambda aa, bb: aa - bb)  # type: ignore


def tup_mul[T](a: tuple[T, ...], b: tuple[T, ...]) -> tuple[T, ...]:
    return tup_op(a, b, lambda aa, bb: aa * bb)  # type: ignore


def tup_div[T](a: tuple[T, ...], b: tuple[T, ...]) -> tuple[T, ...]:
    return tup_op(a, b, lambda aa, bb: aa / bb)  # type: ignore


def tup_idiv(a: tuple[int, ...], b: tuple[int, ...]) -> tuple[int, ...]:
    return tup_op(a, b, lambda aa, bb: aa // bb)


def manhattan(a: tuple[int, int], b: tuple[int, int] | None = None) -> int:
    ax, ay = a
    bx, by = b if b is not None else (0, 0)
    return abs(ax - bx) + abs(ay - by)


def print_set_grid(
    grid: set[tuple[int, int]] | dict[tuple[int, int], str | int],
    empty: str = ".",
    min_x: int | None = None,
    min_y: int | None = None,
    max_x: int | None = None,
    max_y: int | None = None,
):
    xs, ys = zip(*grid)
    minx: int = min(xs) if min_x is None else min_x
    maxx: int = max(xs) if max_x is None else max_x - 1
    miny: int = min(ys) if min_y is None else min_y
    maxy: int = max(ys) if max_y is None else max_y - 1

    get = (
        (lambda k: "#" if k in grid else empty)
        if isinstance(grid, set)
        else (lambda k: str(grid.get(k, empty)))  # type: ignore
    )

    for y in range(miny, maxy + 1):
        line = ""
        for x in range(minx, maxx + 1):
            line += get((x, y))
        print(line)


def print_grid(grid: Sequence[Sequence[str | int]], transpose=False):
    if not transpose:
        for line in grid:
            print("".join(map(str, line)))
    else:
        for y in range(len(grid[0])):
            for x in range(len(grid)):
                print(grid[x][y], end="")
            print()


def ints(src: str | Iterable[str]) -> list[int]:
    match src:
        case str(s):
            return list(map(int, re.findall(r"[+-]?\d+", s)))
        case src:
            assert hasattr(src, "__iter__")
            return list(map(int, src))


class Range:
    """Range [start, end)"""

    def __init__(self, start: int, end: int):
        assert start < end
        self.start = start
        self.end = end

    def overlap(self, other: Range) -> Range | None:
        ostart = max(self.start, other.start)
        oend = min(self.end, other.end)

        if ostart < oend:
            return Range(ostart, oend)

    def __add__(self, v: int) -> Range:
        return Range(self.start + v, self.end + v)

    def __sub__(self, v: int) -> Range:
        return self + (-v)

    def __repr__(self) -> str:
        return f"{self.start}..{self.end}"

    def len(self) -> int:
        return self.end - self.start


def unindent(s: str) -> str:
    return "\n".join(l.lstrip() for l in s.splitlines())


def transpose[T](a: Sequence[Sequence[T]]) -> tuple[tuple[T, ...], ...]:
    return tuple(zip(*a))


def modular_inverse(a: int, m: int) -> int | None:
    """Calculate the modular inverse of a % m, extended euclidean algorithm

    `a` and `m` must be coprime

    Complexity O(min(log(a), log(m))
    """
    t, nt = 0, 1
    r, nr = m, a

    while nr != 0:
        q = r // nr
        t, nt = nt, t - q * nt
        r, nr = nr, r - q * nr

    if r > 1:
        return None

    return t


def chinese_remainder(congruences: list[tuple[int, int]]) -> tuple[int, int]:
    """Calculate the chinese remainder of a list of congruences.

    Args:
        congruences:
            A list of congruences. All moduli must be coprime.

            For example: `[(3, 5), (5, 7)]`

    Returns:
        The chinese remainder
    """
    M = math.prod(m for _, m in congruences)
    solution = 0
    for a, m in congruences:
        Mi = M // m
        Ni = modular_inverse(Mi, m)
        assert Ni is not None
        solution = (solution + a * Mi % M * Ni) % M

    return solution, M


def flatten[T](iter: Iterable[Iterable[T]]) -> list[T]:
    return [v for iter2 in iter for v in iter2]


def shoelace(points: list[Coord]) -> int:
    return (
        sum(
            ax * by - ay * bx
            for (ax, ay), (bx, by) in zip(points, points[1:] + [points[0]])
        )
        // 2
    )


class Vec3[T: (int, float)]:
    def __init__(self, x: T, y: T, z: T):
        self.__x = x
        self.__y = y
        self.__z = z

    @property
    def x(self) -> T:
        return self.__x

    @property
    def y(self) -> T:
        return self.__y

    @property
    def z(self) -> T:
        return self.__z

    def __add__(self, other: Vec3[T]) -> Vec3[T]:
        return Vec3(self.__x + other.__x, self.__y + other.__y, self.__z + other.__z)

    def __sub__(self, other: Vec3[T]) -> Vec3[T]:
        return Vec3(self.__x - other.__x, self.__y - other.__y, self.__z - other.__z)

    def __mul__(self, other: Vec3[T]) -> Vec3[T]:
        return Vec3(self.__x * other.__x, self.__y * other.__y, self.__z * other.__z)

    def __div__(self, other: Vec3[T]) -> Vec3[float]:
        return Vec3(self.__x / other.__x, self.__y / other.__y, self.__z / other.__z)

    def __floordiv__(self, other: Vec3[T]) -> Vec3[T]:
        return Vec3(self.__x // other.__x, self.__y // other.__y, self.__z // other.__z)

    def __lt__(self, other: Vec3[T]) -> bool:
        return self.to_tuple() < other.to_tuple()

    def __le__(self, other: Vec3[T]) -> bool:
        return self.to_tuple() <= other.to_tuple()

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Vec3):
            return False

        return self.to_tuple() == other.to_tuple()

    def __ne__(self, other: object) -> bool:
        return not (self == other)

    def __ge__(self, other: Vec3[T]) -> bool:
        return self.to_tuple() >= other.to_tuple()

    def __gt__(self, other: Vec3[T]) -> bool:
        return self.to_tuple() > other.to_tuple()

    def __abs__(self) -> Vec3[float]:
        return Vec3(abs(self.__x), abs(self.__y), abs(self.__z))

    def __hash__(self) -> int:
        return hash(self.to_tuple())

    def __repr__(self) -> str:
        return f"Vec3({self.__x}, {self.__y}, {self.__z})"

    def to_tuple(self) -> tuple[T, T, T]:
        return (self.__x, self.__y, self.__z)

    @staticmethod
    def from_tuple(tup: tuple[T, T, T]) -> Vec3[T]:
        return Vec3(*tup)

    def length(self) -> float:
        return math.hypot(self.__x, self.__y, self.__z)

    def distance(self, other: Vec3[T]) -> float:
        return (other - self).length()

    def manhattan(self) -> T:
        return self.__x + self.__y + self.__z


def sign[T: (int, float)](a: T) -> int:
    if a < 0:
        return -1
    if a > 0:
        return 1
    return 0


def spaceship[T: (int, float)](a: T, b: T) -> int:
    return sign(a - b)
