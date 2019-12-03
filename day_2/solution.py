import operator
from typing import List, Dict, Callable, TypeVar, Optional

T = TypeVar("T")

SUBROUTINES = {1: operator.add, 2: operator.mul}

with open("input_data") as file:
    input_code = [int(substr) for substr in file.read().strip().split(",")]


def unwind(
    code: List[int],
    routines: Dict[int, Callable[[T, T], T]] = SUBROUTINES,
    debug_hook: Optional[Callable[[int, int, int, int, int, List[int]], None]] = None,
) -> List[int]:
    debug_hook = debug_hook or (lambda *_: None)
    index = 0
    while (op := code[index]) != 99:
        left_arg = code[index + 1]
        right_arg = code[index + 2]
        dst = code[index + 3]
        func = routines[op]
        debug_hook(index, left_arg, right_arg, dst, op, code)
        code[dst] = func(code[left_arg], code[right_arg])
        index += 4
    debug_hook(index, 0, 0, 0, op, code)


def main(**kwargs):
    code = input_code.copy()
    code[1] = 12
    code[2] = 2
    unwind(code, **kwargs)


if __name__ == "__main__":
    main()
