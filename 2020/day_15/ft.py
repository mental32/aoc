import array
from typing import List, Tuple

seq = "12,20,0,6,1,17,7"

preamble = list(map(int, seq.split(",")))

def solve(preamble: List[int], t: int) -> int:
    last = preamble[-1]
    memory: List[Tuple[int, int]] = [(0, 0) for _ in range(t)]

    for (_, n) in enumerate(preamble, start=1):
        memory[n] = (_, _)

    start = len(preamble) + 1
    stop = t + 1

    for idx in range(start, stop):
        (b, a) = memory[last]
        last = a - b
        (_, top) = memory[last]
        memory[last] = (top or idx, idx)

    return last


print(f"Part1: {solve(preamble, 2020)}")
print(f"Part2: {solve(preamble, 30_000_000)}")
