from collections import defaultdict
from pathlib import Path


p = Path(__file__).parent.joinpath("tw.txt").absolute()

c = {
    0: "n",
    90: "e",
    180: "s",
    270: "w",
}


def move(n: int, d: str, p: dict):
    o = {"n": "s", "e": "w", "s": "n", "w": "e"}

    p[o[d]] -= n

    if p[o[d]] < 0:
        p[d] += -p[o[d]]
        p[o[d]] = 0


def part1():
    f = 90

    m = {
        "e": 0,
        "n": 0,
        "s": 0,
        "w": 0,
    }

    for line in map(str.strip, open(p)):
        i, _ = line[0], line[1:]
        d = int(_)

        if i in "NEWS":
            move(d, i.lower(), m)

        elif i == "L":
            f = (f - d) % 360

        elif i == "R":
            f = (f + d) % 360

        elif i == "F":
            move(d, c[f], m)

    print(sum(map(abs, m.values())))  # type: ignore


part1()


def part2():
    NEWS = "NEWS"

    wx, wy = 10, 1
    x, y = 0, 0

    for line in map(str.strip, open(p)):
        i, _ = line[0], line[1:]
        d = int(_)

        if i == "N":
            wy += d

        elif i == "S":
            wy -= d

        elif i == "E":
            wx += d

        elif i == "W":
            wx -= d

        elif i == "L":
            for _ in range(d // 90):
                wx, wy = -wy, wx

        elif i == "R":
            for _ in range(d // 90):
                wx, wy = wy, -wx

        elif i == "F":
            x += wx * d
            y += wy * d

    print(abs(x) + abs(y))  # type: ignore

part2()
