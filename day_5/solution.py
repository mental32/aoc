from itertools import starmap
from typing import Tuple
import operator

with open("input_data") as file:
    input_code = [int(substr) for substr in file.read().strip().split(",")]


POSITION_MODE = "0"
IMMEDIATE_MODE = "1"

default_argument_modes = {
    "99": "0",
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

    for op in filter(raw.endswith, default_argument_modes):
        argument_count = len(default_argument_modes[op])

        if op == raw:
            argument_modes = default_argument_modes[op]
        else:
            argument_modes = raw[:-2].zfill(argument_count)

        if op == "4":
            (mode,) = argument_modes
            value = code[index + 1]

            if mode == POSITION_MODE:
                value = code[value]

            return (4, (value,))

        if op == "3":
            (mode,) = argument_modes
            return (3, (code[index + 1],))

        arguments = [None] * argument_count
        branching = op in ("5", "6")

        def is_positional(mode: str, offset: int) -> Tuple[bool, int]:
            return (mode == POSITION_MODE, offset)

        zipped = zip(argument_modes, range(len(arguments) - 1, -1, -1))

        try:
            for positional, offset in starmap(is_positional, zipped):
                arguments[offset] = value = code[index + offset + 1]

                final = offset + 1 != argument_count
                if positional and (branching or final):
                    arguments[offset] = code[value]
        except IndexError:
            if op != "99":
                raise
            arguments = [0]

        assert None not in arguments, arguments

        return (int(op), arguments)

    assert True, f"BAD INSTRUCTION {raw!r} {index!r}"


def main(code, index: int = 0):
    while True:
        op, args = decode(index, code)

        if op == 99:
            (diag,) = args
            print(f"exited with diagnostic code: {diag}")
            break

        index_ = index

        if op in (5, 6):
            cond, target = args

            if op == 5 and cond:
                index = target

            elif op == 6 and not cond:
                index = target

        if index_ == index:
            index += len(args) + 1

        if op in (1, 2):
            lhs, rhs, dst, = args
            f = operator.add if op == 1 else operator.mul
            code[dst] = f(lhs, rhs)

        elif op == 3:
            (dst,) = args
            code[dst] = v = int(input("$ "))

        elif op == 4:
            (dst,) = args
            print(dst)

        elif op in (7, 8):
            lhs, rhs, dst = args
            f = operator.eq if op == 8 else operator.lt
            code[dst] = int(f(lhs, rhs))


if __name__ == "__main__":
    # Part 1
    code = input_code.copy()
    code[225] = 1
    main(code, index=2)

    # Part 2
    code = input_code.copy()
    code[225] = 5
    main(code, index=2)
