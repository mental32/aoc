import math
from pathlib import Path
from typing import Deque, Dict, Iterable, List, Optional, Set, Tuple, Union

p = Path(__file__).parent.joinpath("s.txt").absolute()

assert p.exists()

import re

rules: Dict[str, Dict[str, int]] = {}

pate = re.compile(r"^(.+) bags contain (.+).$")
pq = re.compile(r"^(\d+) (.+) bags?$")

assert (
    pate.fullmatch(
        "shiny brown bags contain 4 clear purple bags, 5 striped chartreuse bags, 5 pale lavender bags."
    )
    is not None
)

# Parsing

source = p.read_text().strip()

for line in source.split("\n"):
    mat = pate.fullmatch(line)
    assert mat is not None, line

    [lhs, rhs] = mat.groups()

    if rhs == "no other bags":
        rules[lhs] = {}
        continue

    def _(rhs: str):
        for part in rhs.split(", "):
            match = pq.fullmatch(part)
            assert match is not None, part

            [n, k] = match.groups()

            yield int(n), k

    rules[lhs] = {st: n for (n, st) in _(rhs)}

# Part 1

def k_contains(*bags: str, _n: Optional[Set[str]] = None) -> int:
    n = _n or set()

    for a, b in rules.items():
        if a in n:
            continue

        bs = set(b)
        union = bs & n

        if union or (set(bags) & bs):
            print(union, (set(bags) & bs))
            n.add(a)
            k_contains(*[*bags, a], _n=n)

    return len(n)


print(k_contains("shiny gold"))

# Part 2

def bag_sum(bag: str) -> int:
    assert bag in rules
    k = 0

    req = rules[bag]

    from collections import deque
    queue = deque(req.items())

    while queue:
        s, n = queue.popleft()

        for _ in range(n):
            k += 1

            if s in rules:
                req = rules[s]
                queue.extend(req.items())

    return k

print(bag_sum("shiny gold"))
