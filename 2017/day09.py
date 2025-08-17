from aocd.models import Puzzle

puzzle = Puzzle(2017, int("09"))

data = puzzle.input_data


def parse(s: str, i: int, depth=1) -> tuple[int, int, int]:
    if s[i] == "<":
        i += 1
        n_chars = 0
        while s[i] != ">":
            if s[i] == "!":
                i += 2
            else:
                i += 1
                n_chars += 1

        return i + 1, 0, n_chars
    elif s[i] == "{":
        i += 1
        score = depth
        n_chars = 0
        while True:
            if s[i] == "}":
                break

            i, v, chrs = parse(s, i, depth=depth + 1)
            score += v
            n_chars += chrs

            if s[i] == ",":
                i += 1
        return i + 1, score, n_chars
    else:
        print(s[i])
        raise Exception()


_, puzzle.answer_a, puzzle.answer_b = parse(data, 0)
