from __future__ import annotations

import heapq
import math
import re
from collections import deque
from copy import deepcopy
from typing import Callable, Iterable, Iterator, Sequence, cast, overload

type Coord = tuple[int, int]


class Grid[T]:
    def __init__(self, cols: int, rows: int, _grid: list[list[T]]):
        self._cols = cols
        self._rows = rows
        # Index by _grid[y][x]
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

    def __contains__(self, coord: Coord | Vec2[int]) -> bool:
        match coord:
            case int(x), int(y):
                return self.is_inside(x, y)
            case Vec2(x, y):
                return self.is_inside(x, y)
            case _:
                raise Exception("Unexpected argument")

    def get[E](self, x: int, y: int, default: E = None) -> T | E:
        """Get an item in the grid

        Args:
            x: The x position
            y: The y position

        Returns:
            The item
        """
        if (x, y) not in self:
            return default

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
    def __getitem__(self, index: tuple[int, int] | Vec2[int]) -> T: ...

    @overload
    def __getitem__(self, index: tuple[slice, int] | tuple[int, slice]) -> list[T]: ...

    @overload
    def __getitem__(self, index: tuple[slice, slice]) -> Grid[T]: ...

    def __getitem__(
        self, index: tuple[int | slice, int | slice] | Vec2[int]
    ) -> T | list[T] | Grid[T]:
        """Get values in the grid

        Args:
            coord: (x, y) coordinate
        """
        match index:
            case int(x), int(y):
                return self._grid[y][x]
            case Vec2(x, y):
                return self._grid[y][x]
            case x, int(y) if isinstance(x, slice):
                x_start = x.start if x.start is not None else 0
                x_stop = x.stop if x.stop is not None else self.cols
                x_step = x.step if x.step is not None else 1

                return [self._grid[y][xx] for xx in range(x_start, x_stop, x_step)]
            case int(x), y if isinstance(y, slice):
                y_start = y.start if y.start is not None else 0
                y_stop = y.stop if y.stop is not None else self.rows
                y_step = y.step if y.step is not None else 1

                return [self._grid[yy][x] for yy in range(y_start, y_stop, y_step)]
            case x, y if isinstance(x, slice) and isinstance(y, slice):
                x_start = x.start if x.start is not None else 0
                x_stop = x.stop if x.stop is not None else self.cols
                x_step = x.step if x.step is not None else 1

                y_start = y.start if y.start is not None else 0
                y_stop = y.stop if y.stop is not None else self.rows
                y_step = y.step if y.step is not None else 1

                return Grid.from_2d_list(
                    [
                        [self._grid[yy][xx] for xx in range(x_start, x_stop, x_step)]
                        for yy in range(y_start, y_stop, y_step)
                    ],
                )
            case _:
                raise Exception("Invalid arguments")

    def __setitem__(
        self, index: tuple[int | slice, int | slice] | Vec2[int], v: T
    ) -> None:
        """Set an item in the grid

        Args:
            coord: (x, y) coordinate
            v: Value
        """
        match index:
            case int(x), int(y):
                self.set(x, y, v)
            case Vec2(x, y):
                self.set(x, y, v)
            case x, int(y) if isinstance(x, slice):
                x_start = x.start if x.start is not None else 0
                x_stop = x.stop if x.stop is not None else self.cols
                x_step = x.step if x.step is not None else 1

                for xx in range(x_start, x_stop, x_step):
                    self.set(xx, y, v)
            case int(x), y if isinstance(y, slice):
                y_start = y.start if y.start is not None else 0
                y_stop = y.stop if y.stop is not None else self.rows
                y_step = y.step if y.step is not None else 1

                for yy in range(y_start, y_stop, y_step):
                    self.set(x, yy, v)
            case x, y if isinstance(x, slice) and isinstance(y, slice):
                x_start = x.start if x.start is not None else 0
                x_stop = x.stop if x.stop is not None else self.cols
                x_step = x.step if x.step is not None else 1

                y_start = y.start if y.start is not None else 0
                y_stop = y.stop if y.stop is not None else self.rows
                y_step = y.step if y.step is not None else 1

                for xx in range(x_start, x_stop, x_step):
                    for yy in range(y_start, y_stop, y_step):
                        self.set(xx, yy, v)
            case _:
                raise Exception("Invalid arguments")

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
    def new_fill(fill: Callable[[], T], cols: int, rows: int) -> Grid[T]:
        return Grid.from_2d_list([[fill() for _ in range(cols)] for _ in range(rows)])

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
    ) -> Iterator[tuple[int, int]]:
        for x in range(self.cols):
            for y in range(self.rows):
                v = self[x, y]
                if f((x, y), v):
                    yield x, y

    def find_value(self, v: T) -> Iterator[tuple[int, int]]:
        return self.find(lambda _, vv: v == vv)

    def neighbours4(
        self, coord: tuple[int, int], inside: bool = True
    ) -> Iterator[tuple[int, int]]:
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
    ) -> Iterator[tuple[int, int]]:
        x, y = coord

        for dx in [-1, 0, 1]:
            for dy in [-1, 0, 1]:
                if dx == dy == 0:
                    continue

                nx = x + dx
                ny = y + dy

                if (not inside) or self.is_inside(nx, ny):
                    yield nx, ny

    def coords(self) -> Iterator[tuple[int, int]]:
        for x in range(self.cols):
            for y in range(self.rows):
                yield x, y

    def values(self) -> Iterator[T]:
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


def clear_term():
    print("\033[2J")


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

    lines = []
    for y in range(miny, maxy + 1):
        line = ""
        for x in range(minx, maxx + 1):
            line += get((x, y))
        lines.append(line)
    print("\n".join(lines))


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


class Vec2[T: (int, float)]:
    @overload
    def __init__(self, x: T, y: T): ...

    @overload
    def __init__(self, x: tuple[T, T]): ...

    def __init__(self, x: T | tuple[T, T], y: T | None = None):
        match (x, y):
            case ((xx, yy), None):
                self.__x: T = xx
                self.__y: T = yy
            case (x, y):
                self.__x: T = cast(T, x)
                self.__y: T = cast(T, y)
            case _:
                raise Exception("Unexpected constructor")

    __match_args__ = ("x", "y")

    @property
    def x(self) -> T:
        return self.__x

    @property
    def y(self) -> T:
        return self.__y

    def __add__(self, other: Vec2[T]) -> Vec2[T]:
        return Vec2(self.__x + other.__x, self.__y + other.__y)

    def __sub__(self, other: Vec2[T]) -> Vec2[T]:
        return Vec2(self.__x - other.__x, self.__y - other.__y)

    def __mul__(self, other: Vec2[T]) -> Vec2[T]:
        return Vec2(self.__x * other.__x, self.__y * other.__y)

    def __div__(self, other: Vec2[T]) -> Vec2[float]:
        return Vec2(self.__x / other.__x, self.__y / other.__y)

    def __floordiv__(self, other: Vec2[T]) -> Vec2[T]:
        return Vec2(self.__x // other.__x, self.__y // other.__y)

    def __lt__(self, other: Vec2[T]) -> bool:
        return self.to_tuple() < other.to_tuple()

    def __le__(self, other: Vec2[T]) -> bool:
        return self.to_tuple() <= other.to_tuple()

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Vec2):
            return False

        return self.to_tuple() == other.to_tuple()

    def __ne__(self, other: object) -> bool:
        return not (self == other)

    def __ge__(self, other: Vec2[T]) -> bool:
        return self.to_tuple() >= other.to_tuple()

    def __gt__(self, other: Vec2[T]) -> bool:
        return self.to_tuple() > other.to_tuple()

    def __abs__(self) -> Vec2[T]:
        return Vec2(cast(T, abs(self.__x)), cast(T, abs(self.__y)))

    def __hash__(self) -> int:
        return hash(self.to_tuple())

    def __repr__(self) -> str:
        return f"Vec2({self.__x}, {self.__y})"

    def to_tuple(self) -> tuple[T, T]:
        return (self.__x, self.__y)

    @property
    def tup(self) -> tuple[T, T]:
        return (self.__x, self.__y)

    @staticmethod
    def from_tuple(tup: tuple[T, T]) -> Vec2[T]:
        return Vec2(*tup)

    def length(self) -> float:
        return math.hypot(self.__x, self.__y)

    def distance(self, other: Vec2[T]) -> float:
        return (other - self).length()

    def manhattan(self) -> T:
        return cast(T, abs(self.__x) + abs(self.__y))

    def updated(self, x: int | None = None, y: int | None = None):
        return Vec2(
            self.x if x is None else x,
            self.y if y is None else y,
        )

    def rot_r(self, y_down=True) -> Vec2[T]:
        if not y_down:
            return Vec2(self.y, -self.x)

        return Vec2(-self.y, self.x)

    def rot_l(self, y_down=True) -> Vec2[T]:
        if not y_down:
            return Vec2(-self.y, self.x)

        return Vec2(self.y, -self.x)

    @staticmethod
    def delta_4() -> tuple[Vec2, ...]:
        return (Vec2(1, 0), Vec2(0, 1), Vec2(-1, 0), Vec2(0, -1))

    @staticmethod
    def delta_8() -> tuple[Vec2, ...]:
        return (
            Vec2(1, 0),
            Vec2(1, 1),
            Vec2(0, 1),
            Vec2(-1, 1),
            Vec2(-1, 0),
            Vec2(-1, -1),
            Vec2(0, -1),
            Vec2(1, -1),
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
        return cast(T, abs(self.__x) + abs(self.__y) + abs(self.__z))

    def updated(self, x: int | None = None, y: int | None = None, z: int | None = None):
        return Vec3(
            self.x if x is None else x,
            self.y if y is None else y,
            self.z if z is None else z,
        )


def sign[T: (int, float)](a: T) -> int:
    if a < 0:
        return -1
    if a > 0:
        return 1
    return 0


def spaceship[T: (int, float)](a: T, b: T) -> int:
    return sign(a - b)


def chunk[T](
    iterable: Iterable[T], size: int, throw_on_cut=True
) -> Iterable[tuple[T, ...]]:
    l: list[T] = []

    it = iter(iterable)

    while True:
        try:
            v = next(it)

            l.append(v)

            if len(l) == size:
                yield tuple(l)
                l.clear()
        except StopIteration:
            if throw_on_cut and l != []:
                raise Exception("Could not split iterable into equal sized chunks")
            break


def sliding_window[T](iterable: Iterable[T], size: int) -> Iterable[tuple[T, ...]]:
    window = deque()

    it = iter(iterable)

    while True:
        try:
            v = next(it)
            window.append(v)

            if len(window) > size:
                window.popleft()

            if len(window) == size:
                yield tuple(window)
        except StopIteration:
            break


class Heap[T]:
    def __init__(self, iterable: Iterable[T] = []):
        self.__queue = list(iterable)
        heapq.heapify(self.__queue)

    def push(self, v: T):
        heapq.heappush(self.__queue, v)

    def pop(self) -> T:
        return heapq.heappop(self.__queue)

    def __bool__(self) -> bool:
        return len(self.__queue) != 0

    def __repr__(self) -> str:
        if self.__queue:
            return f"Heap([{self.__queue[0]}])"
        else:
            return "Heap([])"


class Mat:
    """A matrix superior to numpy because it doesn't overflow"""

    def __init__(self, values: list[list[int]]):
        self.values = values

        self.rows = len(self.values)
        self.cols = len(self.values[0])

    def __repr__(self) -> str:
        col_widths = [
            max(len(str(self[y, x])) for y in range(self.rows))
            for x in range(self.cols)
        ]

        return (
            "["
            + "\n ".join(
                "["
                + ", ".join(
                    str(self[y, x]).rjust(col_widths[x]) for x in range(self.cols)
                )
                + "]"
                for y in range(self.rows)
            )
            + "]"
        )

    def __matmul__(self, other: Mat) -> Mat:
        cols = other.cols
        rows = self.rows

        result = Mat([[0 for _ in range(cols)] for _ in range(rows)])

        for x in range(cols):
            for y in range(rows):
                n = len(self.values[y])
                assert len(other.values) == n

                for i in range(n):
                    result[y, x] += self[y, i] * other[i, x]

        return result

    def __mod__(self, mod: int) -> Mat:
        return Mat([[(v % mod) for v in row] for row in self.values])

    def pow_mod(self, n: int, mod: int) -> Mat:
        assert n >= 0

        if n == 0:
            return Mat([[1, 0], [0, 1]])

        res = self.pow_mod(n // 2, mod)

        if n % 2 == 0:
            return (res @ res) % mod
        else:
            return (res @ res @ self) % mod

    def __mul__(self, scalar: int) -> Mat:
        return Mat([[v * scalar for v in row] for row in self.values])

    def det(self) -> int:
        assert self.cols == 2 and self.rows == 2
        return self[0, 0] * self[1, 1] - self[0, 1] * self[1, 0]

    def modular_inverse(self, mod: int) -> Mat:
        assert self.cols == 2 and self.rows == 2
        return Mat([[self[1, 1], -self[0, 1]], [-self[1, 0], self[0, 0]]]) * pow(
            self.det(), -1, mod
        )

    def __getitem__(self, coord: tuple[int, int]) -> int:
        y, x = coord
        return self.values[y][x]

    def __setitem__(self, coord: tuple[int, int], v: int):
        y, x = coord
        self.values[y][x] = v
