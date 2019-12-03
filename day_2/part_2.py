from itertools import permutations

from solution import input_code, unwind


for a in range(1, 100):
    for b in range(1, 100):
        code = input_code.copy()

        code[1] = a
        code[2] = b
        unwind(code)

        if code[0] == 19690720:
            print(100 * a + b)
            break
