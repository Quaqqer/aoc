from __future__ import annotations

from typing import Any, Callable, Generator, Generic, TypeVar, overload

T = TypeVar("T")
U = TypeVar("U")


class Grid(Generic[T]):
    def __init__(self, cols: int, rows: int, _grid: list[list[T]]):
        self._cols = cols
        self._rows = rows
        self._grid = _grid

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
    def max_x(self) -> int:
        return self._cols

    @property
    def rows(self) -> int:
        return self._rows

    @property
    def max_y(self) -> int:
        return self._cols

    def is_inside(self, x: int, y: int) -> bool:
        return 0 <= x < self._cols and 0 <= y < self._rows

    def is_outside(self, x: int, y: int) -> bool:
        return not self.is_inside(x, y)

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
    def __getitem__(self, index: tuple[int, int]) -> T:
        ...

    @overload
    def __getitem__(self, index: tuple[slice, int] | tuple[int, slice]) -> list[T]:
        ...

    @overload
    def __getitem__(self, index: tuple[slice, slice]) -> Grid[T]:
        ...

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
            cols = x.start - x.stop - 1
            rows = y.start - y.stop - 1
            return Grid(
                cols,
                rows,
                [
                    [self.get(xx, yy) for xx in range(x.start, x.stop.x.step)]
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

    def map(self, fn: Callable[[T], U]) -> Grid[U]:
        return Grid(
            self.cols,
            self.rows,
            [[fn(self[x, y]) for x in range(self.cols)] for y in range(self.rows)],
        )

    def map_with_pos(self, fn: Callable[[tuple[int, int], T], U]) -> Grid[U]:
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
        self, coord: tuple[int, int]
    ) -> Generator[tuple[int, int], Any, None]:
        x, y = coord

        for dx, dy in [(-1, 0), (0, -1), (1, 0), (-1, 0)]:
            if dx == dy == 0:
                continue

            nx = x + dx
            ny = y + dy

            if self.is_inside(nx, ny):
                yield nx, ny

    def neighbours8(
        self, coord: tuple[int, int]
    ) -> Generator[tuple[int, int], Any, None]:
        x, y = coord

        for dx in [-1, 0, 1]:
            for dy in [-1, 0, 1]:
                if dx == dy == 0:
                    continue

                nx = x + dx
                ny = y + dy

                if self.is_inside(nx, ny):
                    yield nx, ny

    def coords(self) -> Generator[tuple[int, int], Any, None]:
        for x in range(self.cols):
            for y in range(self.rows):
                yield x, y

    def values(self) -> Generator[T, Any, None]:
        for x, y in self.coords():
            yield self[x, y]
