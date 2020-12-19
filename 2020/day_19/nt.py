from collections import *
from pathlib import Path
from pprint import pformat
from typing import *
from itertools import *

p = Path(__file__).parent.joinpath("nt.txt").absolute()

assert p.exists()

rules, messages = p.read_text().split("\n\n")

Concat = NewType("Concat", tuple)
Exclusive = NewType("Exclusive", list)
Either = Union[Concat, Exclusive]


def _(rule: str) -> "Tuple[int, List[object]]":
    n, p = rule.split(":")
    p = p.strip()

    p_: List[object]

    if '"' in p:
        p_ = [p[1:-1]]
    else:
        p_ = [tuple(map(int, part.strip().split(" "))) for part in p.split("|")]

    return (int(n), p_)


rules_ = {n: p for (n, p) in map(_, rules.split("\n"))}


from collections.abc import Iterable


def flatten(l):
    for el in l:
        if isinstance(el, Iterable) and not isinstance(el, (str, bytes)):
            yield from flatten(el)
        else:
            yield el


def f(k, r) -> Union[Concat, Exclusive, str]:
    if isinstance(k, str):
        return k

    elif isinstance(k, tuple):
        return Concat([f(path, r) for path in k])

    elif isinstance(k, int):
        x: List[object] = []

        if isinstance(r[k][0], str):
            return r[k][0]

        for term in r[k]:
            flat = f(term, r)
            x.append(flat)

        return x

    else:
        assert False


tree = f(0, rules_)

from pprint import pformat

print(pformat(tree))


def matches(st: str) -> bool:
    def _(tree, st, ptr) -> Optional[int]:
        # print("* ", tree)

        if isinstance(tree, str):
            if st[ptr] == tree:
                print(f"OK {st[ptr:]!r}")
                return ptr + 1
            else:
                print(f"MISMATCH! Expected {tree} got {st[ptr]}")
                return None

        elif isinstance(tree, list):
            # lists are used to denote exclusive terms in a rule.
            # any "one" has to pass.

            print(f"Attempting exclusive rule... {tree}")

            for rule in tree:
                print(f"  * {rule}")

                maybe = _(rule, st, ptr)

                if maybe is not None:
                    print(f"   -> SUCCESS!")
                    return ptr
            else:
                return None

        elif isinstance(tree, tuple):
            # tuples are used to denote concatenated terms.
            # all of the terms must match.

            print(f"Trying concat seq\t  > {tree}")

            for part in tree:
                ptr = _(part, st, ptr)

                if ptr is None:
                    print(f"  -> Concat failed {part}")
                    return None

        return ptr

    r = _(tree, st, 0) is not None

    print(repr(st), r)

    breakpoint()

    return bool(r)


print(sum(map(matches, messages.splitlines())))
