from functools import lru_cache
from typing import Tuple, List
import operator

with open("input_data") as file:
    input_code = [int(substr) for substr in file.read().strip().split(",")]


POSITION_MODE = 0
# IMMEDIATE_MODE = 1

BUILTIN_OPS = {1: operator.add, 2: operator.mul, 7: operator.lt, 8: operator.eq}
BUILTIN_OPS_KEYS = tuple(BUILTIN_OPS)

MODE_GROUPS = (
    ([], ["99"]),
    ([1], ["3"]),
    ([0], ["4"]),
    ([0, 0], ["5", "6"]),
    ([0, 0, 0], ["1", "2", "7", "8"]),
)

DEFAULT_ARGUMENT_MODES = {op: sig for sig, ops in MODE_GROUPS for op in ops}

@lru_cache(maxsize=64)
def parse_raw_mode(mode: str) -> List[int]:
    return [int(char) for char in mode]


def decode(index, code):
    raw = str(code[index])

    if raw == "3":
        return (3, (code[index + 1],))

    if raw == "99":
        return (99, [])

    for op in filter(raw.endswith, DEFAULT_ARGUMENT_MODES):
        argument_modes = DEFAULT_ARGUMENT_MODES[op]
        argument_count = len(argument_modes)

        if op != raw:
            argument_modes = parse_raw_mode(raw[:-2].zfill(argument_count))


        if op == "4":
            value = code[index + 1]

            if POSITION_MODE in argument_modes:
                value = code[value]

            return (4, (value,))

        arguments = [None] * argument_count
        branching = op in ("5", "6")

        zipped = zip(argument_modes[::-1], range(argument_count))

        for immediate, offset in zipped:
            arguments[offset] = value = code[index + offset + 1]

            nonterminal = offset + 1 != argument_count
            if not immediate and nonterminal:
                arguments[offset] = code[value]

        assert None not in arguments, arguments

        if branching:
            arguments[-1] = code[arguments[-1]]

        return (int(op), arguments)

    assert False, f"BAD INSTRUCTION {raw!r} @ {index!r}"


def main(code, input_: List[int], index: int = 0) -> List[int]:
    output = []

    # fmt: off
    io = {
        3: (lambda dst: operator.setitem(code, dst, input_.pop())),
        4: output.append
    }
    # fmt: on

    while True:
        print(f"{index=} {code[index]}, {code[index:index+4]}")

        outp = decode(index, code)
        op, args = outp

        print(f"{index=}, {op=}, {args=}")

        if op == 99:
            return output

        index_ = index

        if op in (5, 6):
            cond, target = args

            if op == 5 and cond:
                index = target

            elif op == 6 and not cond:
                index = target

        if index_ == index:
            index += len(args) + 1

        # # fmt: off
        # index = (
        #     (op == 5 if args[0] else op == 6)
        #     and args[1]
        #     or index + len(args) + 1
        # )
        # # fmt: on

        if op in BUILTIN_OPS_KEYS:
            lhs, rhs, dst, = args
            f = BUILTIN_OPS[op]
            code[dst] = f(lhs, rhs) + 0

        elif op in io:
            (dst,) = args
            print(f"{index=} IO {op=} {output=} {input_=}")
            io[op](dst)
            print(f"{index=} IO {op=} {output=} {input_=}")


if __name__ == "__main__":
    from itertools import cycle, permutations



    largest = -1
    for index, phases in enumerate(permutations(range(5))):
        assert set(phases) == {0, 1, 2, 3, 4}
        carry = 0
        for phase in phases:
            # output = main([3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0], [phase, carry])
            output = main(input_code[:], [carry, phase])
            print(index, output)
            carry = output[-1]

            if carry > largest:
                largest = carry

    print(largest)
