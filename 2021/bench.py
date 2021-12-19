import importlib
import os

for day in range(1, 25 + 1):
    pypath = f"day{day}.py"
    if os.path.exists(pypath):
        importlib.import_module(pypath.removesuffix(".py"))
