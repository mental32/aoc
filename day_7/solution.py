import operator
from asyncio import run as asyncio_run, Queue, create_task, gather, sleep
from contextvars import ContextVar
from dataclasses import dataclass, field
from functools import lru_cache
from itertools import cycle, permutations
from typing import Tuple, List, Dict, Optional

POSITION_MODE = 0

BUILTIN_OPS = {1: operator.add, 2: operator.mul, 7: operator.lt, 8: operator.eq}
BUILTIN_OPS_KEYS = tuple(BUILTIN_OPS)

MODE_GROUPS: List[Tuple[List[int], List[int]]] = [
    ([], [99]),
    ([1], [3]),
    ([0], [4]),
    ([0, 0], [5, 6]),
    ([0, 0, 0], [1, 2, 7, 8]),
]

DEFAULT_ARGUMENT_MODES: Dict[int, List[int]] = {
    op: sig for sig, ops in MODE_GROUPS for op in ops
}

AMP_DEBUG = ContextVar("AMP_DEBUG")


@lru_cache(maxsize=64)
def parse_raw_mode(mode: str) -> List[int]:
    return [int(part) for part in mode]


@lru_cache(maxsize=128)
def parse_arg_modes(word: int) -> Tuple[int, List[int], int]:
    op = word % 10

    argument_modes = DEFAULT_ARGUMENT_MODES[op]
    argument_count = len(argument_modes)

    raw = str(word)
    if str(op) != raw:
        return op, parse_raw_mode(raw[:-2].zfill(argument_count)), argument_count

    return op, argument_modes, argument_count


def decode(word: int, code: List[int], index: int) -> List[int]:
    if word == 3:
        return 3, [code[index + 1]]

    if word == 4:
        return 4, [code[code[index + 1]]]

    op, argument_modes, argument_count = parse_arg_modes(word)

    if op == 4:
        value = code[index + 1]

        if POSITION_MODE in argument_modes:
            value = code[value]

        return 4, [value]

    zipped = zip(argument_modes[::-1], range(argument_count))
    arguments = [None] * argument_count

    for immediate, offset in zipped:
        arguments[offset] = value = code[index + offset + 1]

        # XXX: If we can deterministically deduce the terminal argument we can omit this.
        nonterminal = offset + 1 != argument_count

        if nonterminal and not immediate:
            arguments[offset] = code[value]

    assert None not in arguments, arguments

    if op in (5, 6):  # branching
        arguments[-1] = code[arguments[-1]]

    return op, arguments


async def intcode_execute(
    code: List[int], inbound: Queue, outbound: Queue, index: int = 0,
) -> None:
    async def read(dst: int) -> None:
        code[dst] = await inbound.get()

    async def write(value: int) -> None:
        await outbound.put(value)

    # fmt: off
    io = {
        3: read,
        4: write
    }
    # fmt: on

    # Its a do-while but embeds code indexing logic
    for word in (code[index] for _ in cycle((None,))):
        await sleep(0)

        if word == 99:
            break

        op, args = decode(word, code, index)

        if op in BUILTIN_OPS_KEYS:
            func = BUILTIN_OPS[op]
            lhs, rhs, dst, = args
            code[dst] = func(lhs, rhs) + 0

        elif op in io:
            func = io[op]
            await func(*args)

        # fmt: off
        index = (
            (op == 5 if args[0] else op == 6)
            and args[1]
            or index + len(args) + 1
        )
        # fmt: on


@dataclass
class Amp:
    ident: str
    input: Queue = field(init=False, default_factory=Queue)
    output: Queue = field(init=False, default_factory=Queue)

    async def jitter(self, input_code: List[int]):
        """Run this amp."""
        AMP_DEBUG.set(f"Amp{self.ident}")
        await intcode_execute(
            input_code[:], self.input, self.output,
        )


async def main():
    with open("input_data") as file:
        input_code = [int(substr) for substr in file.read().strip().split(",")]

    # There are five amps in total, chained.
    amps = [Amp("A")]

    for ident in "BCDE":
        amp = Amp(ident)
        amp.input = amps[-1].output
        amps.append(amp)

    amps[0].input = amps[-1].output

    largest = -1

    part_one: bool = False
    lower, upper = (0, 4) if part_one else (5, 9)

    range_set = set(range(lower, upper + 1))

    for phases in permutations(range(lower, upper + 1)):
        print(f"Running amps with: {phases}")
        assert set(phases) == range_set

        for phase, amp in zip(phases, amps):
            await amp.input.put(phase)

        await amps[0].input.put(0)

        await gather(*[create_task(amp.jitter(input_code)) for amp in amps])

        assert not amps[-1].output.empty()

        carry = amps[-1].output.get_nowait()
        if carry > largest:
            largest = carry

    print(largest)


if __name__ == "__main__":
    asyncio_run(main())
