import re
from math import prod
from pathlib import Path
from typing import Any, Dict, Iterator, List, Set, Tuple

path = Path(__file__).parent.joinpath("sxt.txt")

assert path.exists()

RULE = re.compile(r"^([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)")

Ticket = Tuple[int, ...]
Rule = Tuple[str, List[range]]
Rules = Dict[str, List[range]]
PuzzleInput = Tuple[Rules, Ticket, List[Ticket]]


def parse(path: Path) -> PuzzleInput:
    assert path.exists()
    assert path.is_file()

    chunks = path.read_text().strip().split("\n\n")

    [rules_, [_, ticket_], [_, *tickets]] = map(str.splitlines, chunks)

    def parse_ticket(ticket: str) -> Ticket:
        return tuple(map(int, ticket.split(",")))

    our_ticket = parse_ticket(ticket_)
    rules: Rules = {}

    for rule in rules_:
        match = RULE.fullmatch(rule)
        assert match is not None, f"Failed to match a {rule=!r}"

        name, *rest = match.groups()
        a, b, c, d = map(int, rest)

        rules[name] = [range(a, b + 1), range(c, d + 1)]

    return (rules, our_ticket, [parse_ticket(ticket) for ticket in tickets])

def find_invalid_tickets(
    tickets: List[Ticket], rules: Rules
) -> Set[Tuple[int, Ticket]]:
    def is_valid(digit: int):
        def f(rule) -> bool:
            [a, b] = rule
            return digit in a or digit in b

        return f

    return {
        (digit, ticket)
        for ticket in tickets
        for digit in ticket
        if not any(map(is_valid(digit), rules.values()))
    }

def fields(ticket: Ticket, glob: str, assigned: Dict[str, int]) -> Iterator[int]:
    for name, n in assigned.items():
        if name.startswith(glob):
            yield ticket[n]

def invalidate(
    apparent: str,
    column: int,
    unassigned: Dict[int, Set[str]],
    assigned: Dict[str, int],
):
    group = unassigned[column]

    if apparent not in group:
        return

    group.remove(apparent)

    if len(group) > 1:
        return

    found = group.pop()

    for col in unassigned:
        invalidate(found, col, unassigned, assigned)
    else:
        assigned[found] = column

def part1(inp: PuzzleInput) -> Tuple[int, Set[Ticket]]:
    (rules, _, tickets) = inp

    err = 0
    bad = set()

    for n, ticket in find_invalid_tickets(tickets, rules):
        bad.add(ticket)
        err += n

    return err, bad

def part2(inp: PuzzleInput, bad: Set[Ticket]) -> int:
    (rules, our_ticket, tickets) = inp

    # Tickets we care about dont include badly formed ones.
    validated = set(tickets) ^ bad

    # Make sure all tickets have an equal number of columns.
    _ = set(map(len, validated))
    assert len(_) == 1, _
    ncols = _.pop()

    unassigned: Dict[int, Set[str]] = {}
    assigned: Dict[str, int] = {}

    for column in range(ncols):
        unassigned[column] = possabilities = set(rules) ^ set(assigned)

        for ticket in validated:
            digit = ticket[column]

            for rule in possabilities.copy():
                [left, right] = rules[rule]

                if digit in left or digit in right:
                    continue  # Valid

                invalidate(rule, column, unassigned, assigned)

    return prod(fields(our_ticket, "departure", assigned))

inp = parse(path)

(err, bad) = part1(inp)

print(f"Part1: {err}")
print(f"Part2: {part2(inp, bad)}")
