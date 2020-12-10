from collections import deque
from pathlib import Path
from typing import Deque, Iterable, List, Tuple
from itertools import permutations

p = Path(__file__).parent.joinpath("nine.txt").absolute()

assert p.exists()

inp = list(map(int, p.read_text().strip().split("\n")))

def is_valid_product(n: int, preamble: Iterable[int]) -> bool:
    unique = set(preamble)
    
    def check(ab: Tuple[int, ...]) -> bool:
        (a, b) = ab
        return (a + b) == n if a != b else False

    pairs = permutations(unique, 2)

    validated = filter(check, pairs)

    try:
        next(validated)
    except StopIteration:
        return False
    else:
        return True

def part1(inp: List[int]):
    preamble: Deque[int] = deque(inp[:25])

    stream = iter(inp[25:])

    for number in stream:
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
