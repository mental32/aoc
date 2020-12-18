from os import set_blocking
import re
import operator
from typing import Any, Callable, Dict, List, Tuple, Union

ops: Dict[str, Callable[[int, int], int]] = {
    "+": operator.add,
    "-": operator.sub,
    "*": operator.mul,
}

def math(st: List[str]) -> Tuple[int, int]:
    stack: List[Union[int, function]] = []

    ins = iter(st)
    idx = len(st)
    end = None

    def reduce():
        if len(stack) == 3:
            [lhs, op, rhs] = stack
            assert callable(op)
            stack.clear()
            r = op(lhs, rhs)
            # print(f"'{lhs} {op} {rhs}' -> '")
            stack.append(r)

    for idx, char in enumerate(ins):
        if end is not None and idx <= end:
            # breakpoint()
            continue

        if char == "(":
            start = idx + 1
            sexpr = st[start:]
            end, result = math(sexpr)
            end += start
            stack.append(result)

        elif char == ")":
            break

        elif char == " ":
            continue

        else:
            item = int(char) if char.isdigit() else ops[char]
            stack.append(item)

        reduce()

    reduce()

    assert len(stack) == 1, stack

    return idx, stack.pop()

from pathlib import Path

p = Path(__file__).parent.joinpath("el.txt")

def f(_):
    (___, __) = math(_.strip())
    return __

print(sum(map(f, open(p))))

def f2(st):
    ___ = {
        "+": "*",
        "*": "+",
    }

    h = re.sub(r"(\+|\*)", (lambda m: ___[m[1]]), st)

    import ast

    t = ast.parse(h)

    class T(ast.NodeTransformer):
        def visit_Add(self, node: ast.Add) -> Any:
            return ast.Mult()

        def visit_Mult(self, node: ast.Mult) -> Any:
            return ast.Add()

    T().visit(t)

    return eval(ast.unparse(t))

print(sum(map(f2, open(p))))
