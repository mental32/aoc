import operator
from asyncio import run as asyncio_run, Queue, create_task, gather, sleep
from contextlib import suppress
from dataclasses import dataclass, field
from enum import IntEnum
from functools import lru_cache
from itertools import cycle, permutations
from typing import Tuple, List, Dict, Optional

with suppress(ImportError):
    from uvloop import install as _uvloop_install
    _uvloop_install()

POSITION_MODE = 0


class Opcode(IntEnum):
    # BinOps
    Add = 1
    Mul = 2
    # I/O
    Read = 3
    Write = 4
    # Branching
    Jnz = 5
    Jez = 6
    # Boolean logic
    Lt = 7
    Eq = 8
    # Special
    Halt = 99


BUILTIN_OPS = {
    Opcode.Add: operator.add,
    Opcode.Mul: operator.mul,
    Opcode.Lt: operator.lt,
    Opcode.Eq: operator.eq,
}

BUILTIN_OPS_KEYS = tuple(BUILTIN_OPS)

BRANCHING_OPCODES = (Opcode.Jnz, Opcode.Jez)

MODE_GROUPS: List[Tuple[List[int], List[Opcode]]] = [
    ([], [Opcode.Halt]),
    ([1], [Opcode.Read]),
    ([0], [Opcode.Write]),
    ([0, 0], [Opcode.Jnz, Opcode.Jez]),
    ([0, 0, 0], [Opcode.Add, Opcode.Mul, Opcode.Lt, Opcode.Eq]),
]

DEFAULT_ARGUMENT_MODES: Dict[int, List[int]] = {
    op: sig for sig, ops in MODE_GROUPS for op in ops
}


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
    if word == Opcode.Read:
        return Opcode.Read, [code[index + 1]]

    if word == Opcode.Write:
        return Opcode.Write, [code[code[index + 1]]]

    op, argument_modes, argument_count = parse_arg_modes(word)

    if op == Opcode.Write:
        value = code[index + 1]

        if POSITION_MODE in argument_modes:
            value = code[value]

        return Opcode.Write, [value]

    zipped = zip(argument_modes[-1:0:-1], range(argument_count - 1))
    arguments = [None] * argument_count

    for immediate, offset in zipped:
        arguments[offset] = value = code[index + offset + 1]

        if not immediate:
            arguments[offset] = code[value]

    arguments[-1] = code[index + argument_count]

    assert None not in arguments, arguments

    if op in BRANCHING_OPCODES:  # branching
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
        Opcode.Read: read,
        Opcode.Write: write
    }
    # fmt: on

    # Its a do-while that performs a lookup.
    for word in (code[index] for _ in cycle((None,))):
        await sleep(0)

        if word == Opcode.Halt:
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
            (op == Opcode.Jnz if args[0] else op == Opcode.Jez)
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
        assert set(phases) == range_set

        for phase, amp in zip(phases, amps):
            await amp.input.put(phase)

        # Special case, ampA takes an inital argument of 0.
        await amps[0].input.put(0)

        await gather(*[create_task(amp.jitter(input_code)) for amp in amps])

        assert not amps[-1].output.empty()

        carry = amps[-1].output.get_nowait()
        if carry > largest:
            largest = carry

    print(largest)


if __name__ == "__main__":
    asyncio_run(main())
