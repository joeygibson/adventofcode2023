#!/usr/bin/env python3
import functools
import re
import sys
from collections import defaultdict
from typing import Tuple

popular_symbols: dict[str, list[int]] = defaultdict(list)


def find_symbols(lines: list[str]) -> list[Tuple[str, int, int, int]]:
    symbols: list[Tuple[str, int, int, int]] = []

    for line_no, line in enumerate(lines):
        indices = re.finditer(r'[^\d.]', line)
        vals = [(m.group(), line_no, m.start(), m.end()) for m in indices]
        symbols.extend(vals)

    return symbols


def find_numbers(lines: list[str]) -> list[Tuple[int, int, int, int]]:
    numbers: list[Tuple[int, int, int, int]] = []

    for line_no, line in enumerate(lines):
        indices = re.finditer(r'\d+', line)
        vals = [(int(m.group()), line_no, m.start(), m.end()) for m in indices]
        numbers.extend(vals)

    return numbers


def is_num_near_symbol(num: Tuple[int, int, int, int], symbols: list[Tuple[str, int, int, int]]) -> bool:
    found = False
    for sym in symbols:
        cur_found = False

        if num[2] <= sym[2] <= num[3]:
            found = True
            cur_found = True
        if sym[2] == num[2] - 1 or sym[3] == num[3] + 1:
            found = True
            cur_found = True

        if sym[0] == '*' and cur_found:
            popular_symbols[sym].append(num)
            # break

    return found


def find_numbers_near_symbols(symbols: list[Tuple[str, int, int, int]],
                              numbers: list[Tuple[int, int, int, int]]) -> list[int]:
    vals: list[int] = []

    for num in numbers:
        prev_row_symbols = [s for s in symbols if s[1] == num[1] - 1]
        next_row_symbols = [s for s in symbols if s[1] == num[1] + 1]
        cur_row_symbols = [s for s in symbols if s[1] == num[1]]

        if prev_row_symbols and is_num_near_symbol(num, prev_row_symbols):
            vals.append(num[0])
            continue

        if next_row_symbols and is_num_near_symbol(num, next_row_symbols):
            vals.append(num[0])
            continue

        if cur_row_symbols and is_num_near_symbol(num, cur_row_symbols):
            vals.append(num[0])
            continue

    return vals


def part1(lines: list[str]) -> int:
    symbols = find_symbols(lines)
    numbers = find_numbers(lines)
    vals = find_numbers_near_symbols(symbols, numbers)

    return sum(vals)


def part2(lines: list[str]) -> int:
    vals = list(filter(lambda x: len(x) == 2, popular_symbols.values()))

    for i in range(len(vals)):
        vals[i] = list(map(lambda x: x[0], vals[i]))

    for i in range(len(vals)):
        vals[i] = functools.reduce(lambda x, y: x * y, vals[i])

    return sum(vals)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
