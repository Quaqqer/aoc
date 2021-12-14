import importlib
import os

days = ["day_" + str(d) for d in range(1, 26)]


for day in days:
    if os.path.exists(day + "/prg.py"):
        importlib.import_module(day + ".prg")
