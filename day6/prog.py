#!/usr/bin/env python3
import functools
import sys
import unittest


def part1(lines: list[str]) -> int:
    durations = [int(d) for d in lines[0].split(':')[1].split()]
    records = [int(d) for d in lines[1].split(':')[1].split()]

    print(f'durations: {durations}')
    print(f'records: {records}')

    possibilities = []

    for dur, rec in zip(durations, records):
        print(f'dur: {dur}, rec: {rec}')
        poss = 0
        for i in range(dur):
            dur_minus_i = dur - i
            dur_minus_i_times_i = dur_minus_i * i
            if dur_minus_i_times_i > rec:
                # print(f'found: {i}')
                poss += 1

        possibilities.append(poss)

    print(f'possibilities: {possibilities}')
    return functools.reduce(lambda x, y: x * y, possibilities)




def part2(lines: list[str]) -> int:
    pass


class TestDay6(unittest.TestCase):
    def test_part1(self):
        with open('input0.txt') as f:
            lines = f.read().splitlines()

        part1(lines)

    def test_part2(self):
        with open('input0.txt') as f:
            lines = f.read().splitlines()

        part2(lines)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
