import operator
from asyncio import get_event_loop, Queue, gather, sleep
from contextlib import suppress
from enum import IntEnum
from functools import lru_cache
from itertools import cycle, permutations
from sys import maxsize
from typing import Tuple, List, Dict, Union, Callable, TypeVar, TYPE_CHECKING

with suppress(ImportError):
    from uvloop import install as _uvloop_install

    _uvloop_install()

POSITION_MODE = 0
IMMEDIATE_MODE = 1
RELATIVE_MODE = 2

T = TypeVar("T")  # pylint: disable=invalid-name


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
    # Mode adjusting
    RelativeBaseOffset = 9
    # Special
    Halt = 99


class OOMException(Exception):
    pass


OpcodeT = Union[Opcode, int]
BUILTIN_OPS: Dict[OpcodeT, Callable[[T, T], T]] = {
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
    ([0], [Opcode.Write, Opcode.RelativeBaseOffset]),
    ([0, 0], [Opcode.Jnz, Opcode.Jez]),
    ([0, 0, 0], [Opcode.Add, Opcode.Mul, Opcode.Lt, Opcode.Eq]),
]

DEFAULT_ARGUMENT_MODES: Dict[OpcodeT, List[int]] = {
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


def decode(
    word: int, code: List[int], index: int, offset: int
) -> Tuple[OpcodeT, List[int]]:
    op, argument_modes, argument_count = parse_arg_modes(word)
    print(f"{op=} {code[index: index + argument_count]=}")

    if op in (Opcode.Read, Opcode.Write, Opcode.RelativeBaseOffset):
        value = code[index + 1]

        assert len(argument_modes) == 1

        if POSITION_MODE in argument_modes:
            value = code[value]

        elif RELATIVE_MODE in argument_modes:
            value = code[offset + value]

        if op == Opcode.Read:
            # breakpoint()
            assert IMMEDIATE_MODE not in argument_modes, (op, argument_modes)

        if op == Opcode.RelativeBaseOffset:
            print(f"UPDATING RELATIVE BASE OFFSET {offset!r} with {value!r} to {offset + value!r}")
            offset += value

        return offset, Opcode(op), [value]

    zipped = zip(argument_modes[-1:0:-1], range(argument_count - 1))
    arguments: List[Optional[int]] = [None] * argument_count

    for mode, argument_index in zipped:
        arguments[argument_index] = value = code[index + argument_index + 1]

        if mode == POSITION_MODE:
            arguments[argument_index] = code[value]

        elif mode == RELATIVE_MODE:
            arguments[argument_index] = code[offset + value]

    arguments[-1] = code[index + argument_count]

    assert None not in arguments, arguments

    if op in BRANCHING_OPCODES:  # branching
        mode = argument_modes[0]

        value = arguments[-1]

        if mode == POSITION_MODE:
            value = code[arguments[-1]]

        elif mode == RELATIVE_MODE:
            value = code[offset + arguments[-1]]

        arguments[-1] = value

    return offset, op, arguments


async def intcode_execute(
    code: List[int], inbound: Queue, outbound: Queue, index: int = 0,
) -> None:
    async def read(dst: int) -> None:
        value = await inbound.get()
        print(f"READING {value!r} INTO {dst!r}")
        elastic_index((lambda: operator.setitem(code, dst, value)))

    async def write(value: int) -> None:
        print(value)
        await outbound.put(value)

    # fmt: off
    io = {
        Opcode.Read: read,
        Opcode.Write: write
    }
    # fmt: on

    relative_base_offset = 0

    def elastic_index(func):
        nonlocal code

        while True:
            try:
                # breakpoint()
                return func()
            except OOMException as err:
                print(f"OOM ({err!r}) EXTENDING PROGRAM MEMORY SPACE BY {len(code)=!r}")
                code += [0] * len(code)
                continue
            else:
                assert False

    proccessed = 0

    for word in (code[index] for _ in cycle((None,))):
        processed += 1

        print(f"\n{index=} {relative_base_offset=} {word=}")
        await sleep(0)

        if word == Opcode.Halt:
            break

        relative_base_offset, op, args = elastic_index((lambda: decode(word, code, index, relative_base_offset)))
        print(f"{index=} {relative_base_offset=} {op=} {args=}")


        if op in BUILTIN_OPS_KEYS:
            func = BUILTIN_OPS[op]
            lhs, rhs, dst, = args
            rv = func(lhs, rhs) + 0
            elastic_index((lambda: operator.setitem(code, dst, rv)))

        elif op in io:
            func = io[op]  # type: ignore
            await func(*args)  # type: ignore

        # fmt: off
        index = (  # pylint: disable=consider-using-ternary
            (op == Opcode.Jnz if args[0] else op == Opcode.Jez)
            and args[1]
            or index + len(args) + 1
        )
        # fmt: on


async def main():
    inb, outb = Queue(), Queue()
    # input_code = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

    with open("input_data") as file:
        input_code = [int(substr) for substr in file.read().strip().split(",")]

    await inb.put(1)

    await intcode_execute(input_code[:], inb, outb)


if __name__ == "__main__":
    import asyncio

    asyncio.run(main())
