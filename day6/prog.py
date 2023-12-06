#!/usr/bin/env python3
import functools
import sys
import unittest
from math import floor, sqrt


def process_races(durations: list[int], records: list[int]) -> list[int]:
    possibilities = []

    for dur, rec in zip(durations, records):
        poss = 0
        for i in range(dur):
            dur_minus_i = dur - i
            dur_minus_i_times_i = dur_minus_i * i
            if dur_minus_i_times_i > rec:
                poss += 1

        possibilities.append(poss)

    return possibilities


def process_part2(duration: int, record: int) -> list[int]:
    return len(range(floor((duration - sqrt(duration ** 2 - 4 * record)) / 2),
                     floor((duration + sqrt(duration ** 2 - 4 * record)) / 2)))


def part1(lines: list[str]) -> int:
    durations = [int(d) for d in lines[0].split(':')[1].split()]
    records = [int(d) for d in lines[1].split(':')[1].split()]

    possibilities = process_races(durations, records)

    return functools.reduce(lambda x, y: x * y, possibilities)


def part2(lines: list[str]) -> int:
    duration = int(lines[0].split(':')[1].replace(' ', ''))
    record = int(lines[1].split(':')[1].replace(' ', ''))

    return process_part2(duration, record)
    

class TestDay6(unittest.TestCase):
    def test_part1(self):
        with open('input0.txt') as f:
            lines = f.read().splitlines()

        res = part1(lines)

        self.assertEqual(288, res)

    def test_part2(self):
        with open('input0.txt') as f:
            lines = f.read().splitlines()

        res = part2(lines)

        self.assertEqual(71503, res)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
