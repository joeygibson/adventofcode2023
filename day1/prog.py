#!/usr/bin/env python3

import re
import sys


def part1(lines: list[str]) -> int:
    sum = 0
    for line in lines:
        digits = [c for c in line if c.isdigit()]
        print(f'{line} -> {digits}')
        num = int(f'{digits[0]}{digits[-1]}')

        sum += num

    return sum


def possibly_convert_digit(value: str) -> int:
    if value.isdigit():
        return value
    else:
        return {
            'one': '1',
            'two': '2',
            'three': '3',
            'four': '4',
            'five': '5',
            'six': '6',
            'seven': '7',
            'eight': '8',
            'nine': '9',
        }[value]


def part2(lines: list[str]) -> int:
    sum = 0
    for line in lines:
        digits = re.findall(r'(?=(one|two|three|four|five|six|seven|eight|nine|[1-9]))', line)

        first_digit = possibly_convert_digit(digits[0])
        last_digit = possibly_convert_digit(digits[-1])

        num = int(f'{first_digit}{last_digit}')

        print(f'{line} -> {digits} -> {first_digit}, {last_digit} -> {num}')

        sum += num

    return sum


with open(sys.argv[1]) as f:
    lines = f.read().splitlines()

# print(f'part1 -> {part1(lines)}')
print(f'part1 -> {part2(lines)}')
