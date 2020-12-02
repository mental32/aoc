import re
from functools import *
from itertools import *
from typing import *

with open("./two.txt") as inf:
    d = [p.strip().split(":") for p in inf if p.strip()]

def is_ok(policy: str, password: str) -> bool:
    import re

    [mi, mx, ch] = re.match(r"(\d+)-(\d+) ([0-9A-Za-z])", policy).groups()

    return password.count(ch) in range(int(mi), int(mx) + 1)

valid = sum(is_ok(policy, password) for policy, password in d)

print(valid)

def is_ok(policy: str, password: str) -> bool:
    import re

    [mi, mx, ch] = re.match(r"(\d+)-(\d+) ([0-9A-Za-z])", policy).groups()

    mi = int(mi)
    mx = int(mx)

    return (password[mi] == ch and password[mx] != ch) or (password[mx] == ch and password[mi] != ch)

valid = sum(is_ok(policy, password) for policy, password in d)

print(valid)

import timeit

t = timeit.timeit("sum(is_ok(policy, password) for policy, password in d)", globals=globals(), number=10000)

print(t)
