#!/usr/bin/env python3
import enum
import os
import sys
import unittest
from collections import defaultdict, deque

import aocd

# Borrowed mostly from https://github.com/mebeim/aoc/blob/master/2023/solutions/day23.py
# after my solution worked on the test input but not the real input.

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


def neighbors(grid: list[list[str]], r: int, c: int, ignore_slopes: bool = False):
    cell = grid[r][c]

    if ignore_slopes or cell == '.':
        for r, c in ((r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)):
            if grid[r][c] != '#':
                yield r, c
    elif cell == 'v':
        yield (r + 1, c)
    elif cell == '^':
        yield (r - 1, c)
    elif cell == '>':
        yield (r, c + 1)
    elif cell == '<':
        yield (r, c - 1)


def num_neighbors(grid, r, c, ignore_slopes):
    if ignore_slopes or grid[r][c] == '.':
        return sum(grid[r][c] != '#' for r, c in ((r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)))
    return 1


def is_node(grid, rc, src, dst, ignore_slopes):
    return rc == src or rc == dst or num_neighbors(grid, *rc, ignore_slopes) > 2


def adjacent_nodes(grid, rc, src, dst, ignore_slopes):
    q = deque([(rc, 0)])
    seen = set()

    while q:
        rc, dist = q.popleft()
        seen.add(rc)

        for n in neighbors(grid, *rc, ignore_slopes):
            if n in seen:
                continue

            if is_node(grid, n, src, dst, ignore_slopes):
                yield (n, dist + 1)
                continue

            q.append((n, dist + 1))


def graph_from_grid(grid, src, dst, ignore_slopes=False):
    g = defaultdict(list)
    q = deque([src])
    seen = set()

    while q:
        rc = q.popleft()
        if rc in seen:
            continue

        seen.add(rc)

        for n, weight in adjacent_nodes(grid, rc, src, dst, ignore_slopes):
            g[rc].append((n, weight))
            q.append(n)

    return g


def flood_fill(g: list[list[str]], cur: tuple[int, int], dest: tuple[int, int], distance: int = 0,
               seen: set = set()) -> int:
    if cur == dest:
        return distance

    best = 0
    seen.add(cur)

    for neighbor, weight in g[cur]:
        if neighbor in seen:
            continue

        best = max(best, flood_fill(g, neighbor, dest, distance + weight, seen))

    seen.remove(cur)

    return best


def part1(lines: list[str]) -> int:
    grid = parse(lines)
    height, width = len(grid), len(grid[0])

    grid[0][1] = '#'
    grid[height - 1][width - 2] = '#'

    src = (1, 1)
    dest = (height - 2, width - 2)

    g = graph_from_grid(grid, src, dest)
    res = flood_fill(g, src, dest) + 2

    print(res)

    return res


def part2(lines: list[str]) -> int:
    grid = parse(lines)
    height, width = len(grid), len(grid[0])

    grid[0][1] = '#'
    grid[height - 1][width - 2] = '#'

    src = (1, 1)
    dest = (height - 2, width - 2)

    g = graph_from_grid(grid, src, dest, True)
    res = flood_fill(g, src, dest) + 2

    print(res)

    return res


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
