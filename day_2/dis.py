from typing import List

from solution import main


def debug_hook(
    index: int, lhs: int, rhs: int, dst: int, op: int, code: List[int]
) -> None:
    print(f"{op=:02} @ ip={index:02x} :: {dst=:02x} <- ({lhs=:02x}, {rhs=:02x}) ({code[lhs]}), ({code[rhs]})")


main(debug_hook=debug_hook)
