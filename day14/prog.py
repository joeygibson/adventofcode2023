#!/usr/bin/env python3

import sys
import unittest
from copy import deepcopy


def tilt_north_or_south(the_map: list[list[str]], is_south: bool = False) -> list[list[str]]:
    if is_south:
        the_map.reverse()

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

    if is_south:
        the_map.reverse()

    return the_map


def tilt_west_or_east(the_map: list[list[str]], is_east: bool = False) -> list[list[str]]:
    for row_num, row in enumerate(the_map):
        if is_east:
            row.reverse()

        for col_num, col in enumerate(row):
            if col_num == 0:
                # skip the first column
                continue

            if col in '#.':
                continue

            col_to_move_to = None

            col_range = range(col_num)
            for previous_col in reversed(list(col_range)):
                if the_map[row_num][previous_col] in 'O#':
                    # we hit a rock
                    break

                col_to_move_to = previous_col

            if col_to_move_to is not None:
                the_map[row_num][col_to_move_to] = 'O'
                the_map[row_num][col_num] = '.'

        if is_east:
            row.reverse()

    return the_map


def tilt(the_map: list[list[str]]) -> list[list[str]]:
    the_map = tilt_north_or_south(the_map)
    the_map = tilt_west_or_east(the_map)
    the_map = tilt_north_or_south(the_map, is_south=True)
    the_map = tilt_west_or_east(the_map, is_east=True)

    return the_map


def part1(lines: list[str]) -> int:
    the_map = [list(line) for line in lines]

    tilted_map = tilt_north_or_south(the_map.copy())

    total = 0

    for row_num, row in enumerate(tilted_map):
        rock_count = len([1 for rock in row if rock == 'O'])
        total += rock_count * (len(the_map) - row_num)

    return total


def part2(lines: list[str]) -> int:
    the_map = [list(line) for line in lines]

    seen = [the_map]
    cycles = 1_000_000_000
    loop_index = 0
    loop_length = 0

    for i in range(1_000_000_000):
        if i % 100_000 == 0:
            print(f'{i:,}')

        the_map = tilt(deepcopy(the_map))

        if the_map in seen:
            loop_index = seen.index(the_map)
            loop_length = i + 1 - loop_index
            break

        seen.append(the_map)

    final_map = seen[(cycles - loop_index) % loop_length + loop_index]

    total = 0

    for row_num, row in enumerate(final_map):
        rock_count = len([1 for rock in row if rock == 'O'])
        total += rock_count * (len(the_map) - row_num)

    return total


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part2(self.lines)
        self.assertEqual(136, res)

    def test_part2(self):
        res = part2(self.lines)
        self.assertEqual(64, res)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
