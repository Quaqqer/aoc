from aocd.models import Puzzle

puzzle = Puzzle(2017, int("04"))

data = puzzle.input_data

passwords = data.splitlines()


def is_password_valid_a(password: str):
    return len(password.split()) == len(set(password.split()))


def is_password_valid_b(password: str):
    return len(password.split()) == len(
        set("".join(sorted(word)) for word in password.split())
    )


puzzle.answer_a = sum(is_password_valid_a(password) for password in passwords)
puzzle.answer_b = sum(is_password_valid_b(password) for password in passwords)
