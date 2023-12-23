#!/usr/bin/env python3
import enum
import os
import sys
import unittest

import aocd

MODULE_DIR = os.path.dirname(os.path.abspath(__file__))


def download_input(year, day):
    file_name = f'input1.txt'

    if os.path.exists(file_name):
        print(f'Input file {file_name} already exists, skipping download.')
    else:
        print(f'Downloading input file {file_name}.')
        with open(file_name, 'w') as f:
            f.write(aocd.get_data(year=year, day=day))


class Direction(enum.Enum):
    North = '^'
    South = 'v'
    East = '>'
    West = '<'


def parse(lines: list[str]) -> list[list[str]]:
    return [list(line) for line in lines]


def flood_fill(grid: list[list[str]], x: int, y: int, dest_x: int, dest_y: int, dir: Direction) -> int:
    if x == dest_x and y == dest_y:
        grid[y][x] = 'O'
        return 1

    if x < 0 or x >= len(grid[0]) or y < 0 or y >= len(grid):
        return 0

    if grid[y][x] in Direction and grid[y][x] != dir.value:
        return 0

    if grid[y][x] == '#' or grid[y][x] == 'O':
        return 0

    grid[y][x] = 'O'

    north = flood_fill(grid, x, y - 1, dest_x, dest_y, Direction.North)
    south = flood_fill(grid, x, y + 1, dest_x, dest_y, Direction.South)
    east = flood_fill(grid, x + 1, y, dest_x, dest_y, Direction.East)
    west = flood_fill(grid, x - 1, y, dest_x, dest_y, Direction.West)

    # print(f'x: {x}, y: {y}, north: {north}, south: {south}, east: {east}, west: {west}')

    return max(north, south, east, west) + 1


def part1(lines: list[str]) -> int:
    grid = parse(lines)

    res = flood_fill(grid, 1, 0, 21, 22, Direction.South)

    for line in grid:
        print(''.join(line))

    print(res)

    return res


def part2(lines: list[str]) -> int:
    pass


class TestProg(unittest.TestCase):
    def setUp(self):
        download_input(2023, 23)

        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines)
        self.assertEqual(94, res)

    def test_part2(self):
        res = part2(self.lines)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
