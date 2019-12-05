from itertools import starmap
from typing import Tuple, List
import operator

with open("input_data") as file:
    input_code = [int(substr) for substr in file.read().strip().split(",")]


POSITION_MODE = "0"
IMMEDIATE_MODE = "1"

BUILTIN_OPS = {1: operator.add, 2: operator.mul, 7: operator.lt, 8: operator.eq}

DEFAULT_ARGUMENT_MODES = {
    "99": "",
    # BinOp
    "2": "000",
    "1": "000",
    # IO
    "3": "1",
    "4": "0",
    # Branch
    "5": "00",
    "6": "00",
    "7": "000",
    "8": "000",
}


def decode(index, code):
    raw = str(code[index])

    if raw == "3":
        return (3, (code[index + 1],))

    if raw == "99":
        return (99, ())

    for op in filter(raw.endswith, DEFAULT_ARGUMENT_MODES):
        argument_count = len(DEFAULT_ARGUMENT_MODES[op])

        if op == raw:
            argument_modes = DEFAULT_ARGUMENT_MODES[op]
        else:
            argument_modes = raw[:-2].zfill(argument_count)

        if op == "4":
            (mode,) = argument_modes
            value = code[index + 1]

            if mode == POSITION_MODE:
                value = code[value]

            return (4, (value,))

        arguments = [None] * argument_count
        branching = op in ("5", "6")

        def is_positional(mode: str, offset: int) -> Tuple[bool, int]:
            return (mode == POSITION_MODE, offset)

        zipped = zip(argument_modes, range(argument_count - 1, -1, -1))

        for positional, offset in starmap(is_positional, zipped):
            arguments[offset] = value = code[index + offset + 1]

            nonterminal = offset + 1 != argument_count
            if positional and (branching or nonterminal):
                arguments[offset] = code[value]

        assert None not in arguments, arguments

        return (int(op), arguments)

    assert True, f"BAD INSTRUCTION {raw!r} {index!r}"


def main(code, input_: List[int], index: int = 0) -> List[int]:
    output = []

    io = {
        3: (lambda dst: operator.setitem(code, dst, input_.pop())),
        4: output.append
    }

    while True:
        op, args = decode(index, code)

        if op == 99:
            return output

        index_ = index

        if op in (5, 6):
            cond, target = args

            if (op == 5 and cond) or (op == 6 and not cond):
                index = target

        if index_ == index:
            index += len(args) + 1

        if op in (1, 2, 7, 8):
            lhs, rhs, dst, = args
            f = BUILTIN_OPS[op]
            code[dst] = f(lhs, rhs) + 0

        elif op in (3, 4):
            (dst,) = args
            io[op](dst)


if __name__ == "__main__":
    for inp in (1, 5):
        output = main(input_code[:], [inp])
        print(output)
