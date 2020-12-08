import time
from pathlib import Path
from typing import Deque, Dict, Iterable, List, Optional, Set, Tuple, Union

p = Path(__file__).parent.joinpath("eight.txt").absolute()

assert p.exists()

import re

code = list(enumerate(p.read_text().strip().split("\n")))

import sys

def exec(code: List[Tuple[int, str]], *, restrict_double: bool = False) -> int:
    acc = 0
    ip = 0

    trace = set()

    while True:
        if ip >= len(code):
            break

        [idx, ins] = code[ip]    
        assert idx == ip

        # print(f"[{idx}] {ins!r} {acc=!r}")

        if restrict_double and idx in trace:
            raise Exception(acc)

        if ins.startswith("nop"):
            pass

        elif ins.startswith("acc"):
            [_, value] = ins.split(" ")
            acc += int(value)
            del value, _

        if ins.startswith("jmp"):
            [_, value] = ins.split(" ")
            ip += int(value)
            del value, _
        else:
            ip += 1

        trace.add(idx)

    return acc

def force(locs: List[int], f) -> Optional[int]:
    import copy

    for idx in locs:
        clone = copy.deepcopy(code)

        [_, ins] = clone[idx]

        old = ins

        [_, rest] = ins.split(" ")

        clone[idx] = (idx, f(rest))

        # print(f"Replacing {old!r} -> {clone[idx]!r}")

        try:
            acc = exec(clone, restrict_double=True)
        except Exception:
            # print(f"\tDouble restrict!")
            continue
        else:
            return acc

    return None

def part2():
    for (fr, to) in {("nop", "jmp"), ("jmp", "nop")}:
        locs = [idx for idx, instr in code if instr.startswith(fr)]

        if (v := force(locs, (lambda r: to + " " + r))) is not None:
            return v

    assert False

try:
    exec(code, restrict_double=True)
except Exception as exc:
    print(exc)

print(part2())