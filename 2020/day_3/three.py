from itertools import *
from functools import *
from typing import *
from math import *
from pathlib import *

p = Path(__file__).parent.joinpath("three.txt").absolute()

assert p.exists()

input_data = p.read_text().strip().split("\n")
row_length = len(input_data[0])

def solve(delta, start = (0, 0)) -> int:
    dx, dy, = delta
    x, y, = start

    hits = 0

    while True:
        # print(x, y, hits, (l := input_data[y % len(input_data)]), l[x % row_length])
        try:
            hits += input_data[y][x % row_length] == '#'
        except IndexError:
            return hits

        x += dx
        y += dy


def part1() -> int:
    return solve((3, 1), (3, 1)) # part 1


def part2() -> int:
    p2 = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

    n = 1

    for delta in p2:
        t = solve(delta)
        n *= t

    return n

from timeit import timeit

assert part1() == 187, part1()
assert part2() == 4723283400, part2()

print(f"Part1 -> {timeit('part1()', globals=globals())}")
print(f"Part2 -> {timeit('part2()', globals=globals())}")
