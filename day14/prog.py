#!/usr/bin/env python3

import sys
import unittest


def tilt(the_map: list[list[str]]) -> list[list[str]]:
    for row_num, row in enumerate(the_map):
        if row_num == 0:
            # skip the first row
            continue

        for rock_num, rock in enumerate(row):
            if rock in '#.':
                continue

            row_to_move_to = None

            row_range = range(row_num)
            for previous_row in reversed(list(row_range)):
                if the_map[previous_row][rock_num] in 'O#':
                    # we hit a rock
                    break

                row_to_move_to = previous_row

            if row_to_move_to is not None:
                the_map[row_to_move_to][rock_num] = 'O'
                the_map[row_num][rock_num] = '.'

    return the_map


def part1(lines: list[str]) -> int:
    the_map = [list(line) for line in lines]

    tilted_map = tilt(the_map.copy())

    total = 0

    for row_num, row in enumerate(tilted_map):
        rock_count = len([1 for rock in row if rock == 'O'])
        total += rock_count * (len(the_map) - row_num)

    return total


def part2(lines: list[str]) -> int:
    pass


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines)
        self.assertEqual(136, res)

    def test_part2(self):
        res = part2(self.lines)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
