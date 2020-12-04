import re
from itertools import *
from pathlib import *
from functools import *

p = Path(__file__).parent.joinpath("four.txt").absolute()

assert p.exists()


"""

    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)

"""

passports = p.read_text().split("\n\n")

req = { "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" }

def is_valid(p) -> bool:
    rem = list(req)[:]
    for line in p.strip().split("\n"): 
        for substr in line.split(" "):
            left, right = substr.split(":")
            if left in rem:
                rem.remove(left)
                continue

    return not rem

valid = sum(is_valid(p) for p in passports if p.strip())

print(valid)

"""
    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
        If cm, the number must be at least 150 and at most 193.
        If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
    cid (Country ID) - ignored, missing or not.
"""

def _byr(st: str):
    assert re.fullmatch(r"\d{4}", st) is not None, st
    assert int(st) in range(1920, 2003)

def _hcl(st: str):
    assert re.fullmatch(r"#[0-9-a-f]{6}", st) is not None, st

def _pid(st: str):
    assert re.fullmatch(r"\d{9}", st) is not None, st

def _eyr(st: str):
    assert re.fullmatch(r"\d{4}", st) is not None, st
    assert int(st) in range(2020, 2031)

def _hgt(st: str):
    assert (match := re.fullmatch(r"(\d+)(cm|in)", st)) is not None, st
    [k, t] = match.groups()
    assert int(k) in range(150, 194) if t == "cm" else int(k) in range(59, 77)

def _ecl(st: str):
    assert st in {*"amb blu brn gry grn hzl oth".split(" ")}

def _iyr(st: str):
    assert re.fullmatch(r"\d{4}", st) is not None, st
    assert int(st) in range(2010, 2021)

def is_valid_(entry: str) -> bool:
    remaining = list(req)[:]
    for _ in entry.replace("\n", " ").strip().split(" "):
        if _.startswith("cid"):
            continue

        tag, data  = _.split(":")

        try:
            globals()[f"_{tag}"](data)
        except AssertionError:
            return False

        remaining.remove(tag)
    else:
        return not remaining


assert re.match(r"#[0-9a-f]{6}", "#f8824c") is not None

valid = sum(is_valid_(p) for p in passports if p.strip())

print(valid)

