from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor
from re import search

INPUT_DATA = range(353096, 843212)

# Part 1

def match(value: int) -> bool:
    code = str(value)

    assert len(code) == 6

    last = -1
    doubles = 0
    singles = []

    for digit in code:
        cur = int(digit)
        if cur < last:
            return False

        last = cur

        if last not in singles:
            singles.append(last)
        else:
            singles.remove(last)
            doubles += 1

    return bool(doubles)

# Part 2

def match_more(value: int) -> bool:
    code = str(value)
    return list(code) == sorted(code) and 2 in map(code.count, code)

def work(func, part: str) -> None:
    with ThreadPoolExecutor() as executor:
        print(f"Part {part}: Started")
        print(f"Part {part}: {sum(executor.map(func, INPUT_DATA))}")

def main():
    with ProcessPoolExecutor() as executor:
        tasks = [executor.submit(work, func, part) for func, part in zip((match, match_more), "12")]

        for task in tasks:
            task.result()


if __name__ == "__main__":
    main()
