from collections import deque
from pathlib import Path
from typing import Deque, Iterable, List

p = Path(__file__).parent.joinpath("nine.txt").absolute()

assert p.exists()

inp = list(map(int, p.read_text().strip().split("\n")))

def is_valid_product(n: int, s: Iterable[int]) -> bool:
    l = set(s)

    from itertools import permutations

    for (a, b) in permutations(l, 2):
        if a == b:
            continue

        if (a + b) == n:
            return True
    else:
        return False

def part1(inp: List[int]):
    preamble: Deque[int] = deque(map(int, inp[:25]))

    stream = iter(inp[25:])

    for number in map(int, stream):
        assert is_valid_product(number, preamble), number
        preamble.popleft()
        preamble.append(number)


def part2(inp: List[int]) -> int:
    try:
        part1(inp)
    except AssertionError as ae:
        [weakness] = ae.args
    else:
        raise RuntimeError("Expected an XMAS weakness.")

    for si, start in enumerate(inp):
        acc = start
        rn = iter(inp[si + 1:])

        for idx, n in enumerate(rn, start=si + 1):
            acc += n

            if acc == weakness:
                s = inp[si:idx]
                return max(s) + min(s)

            elif acc > weakness:
                break
    else:
        assert False, "unreachable."

try:
    part1(inp)
except AssertionError as ae:
    [p1] = ae.args
else:
    assert False

p2 = part2(inp)

print(f"Part1: {p1}\nPart2: {p2}")
