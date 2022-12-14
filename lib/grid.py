import lib


def neighbours4(pos: tuple[int, int]) -> list[tuple[int, int]]:
    return [lib.tup_add(pos, d) for d in [(0, -1), (0, 1), (-1, 0), (1, 0)]]


def neighbours8(pos: tuple[int, int]) -> list[tuple[int, int]]:
    return [
        lib.tup_add(pos, d)
        for d in [
            (0, -1),
            (1, -1),
            (1, 0),
            (1, 1),
            (0, 1),
            (-1, 1),
            (-1, 0),
            (-1, -1),
        ]
    ]
