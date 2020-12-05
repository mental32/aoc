import math
from pathlib import Path
from typing import Iterable, List

p = Path(__file__).parent.joinpath("five.txt").absolute()

assert p.exists()

def bsp(seq: List[bool], start: int, stop: int) -> int:
    if not seq:
        return start

    head, tail = seq[0], seq[1:]

    # print(f"({seq}) -> {head} [{(start, stop)!r}]")

    half = start + ((stop - start) / 2)

    if head:
        stop = int(half)
    else:
        start = int(half) + 1

    # print(f"({seq}) -> {head} [{rn!r}]\n")

    return bsp(tail, start, stop)


def usid(bp: str) -> int:
    _, __ = bp[:7], bp[-3:]

    rowd = [ch == "F" for ch in _]
    row = bsp(rowd, start=0, stop=127)

    columnd = [ch == "L" for ch in __]
    column = bsp(columnd, start=0, stop=7)

    return row * 8 + column

# assert (v := bsp(list("FBFBBFF"), start=0, stop=127)) == 44, v
# assert (v := bsp("RLR", lh="L", uh="R", rn=range(0, 7))) == 5, v
assert usid("FBFBBFFRLR") == 357

inp = p.read_text().strip().split("\n")

# seat_usids = sorted(list(map(usid, inp)))

# p1 = max(seat_usids)

# partial = seat_usids[:]
# full = range(min(partial), max(partial))

# p2 = set(full).difference(partial).pop()
