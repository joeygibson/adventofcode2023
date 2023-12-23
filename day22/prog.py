#!/usr/bin/env python3
import sys
import unittest
from queue import PriorityQueue


class Brick:
    def __init__(self, start, end):
        self.start = start
        self.end = end
        self.floor = min(self.start[2], self.end[2])
        self.height = abs(self.start[2] - self.end[2])
        self.support = []
        self.supporting = []

    def __repr__(self):
        return f'Brick({self.start}, {self.end})'

    def __lt__(self, other):
        return self.floor < other.floor


def parse(lines: list[str]) -> tuple[PriorityQueue, int | None]:
    bricks = PriorityQueue()
    lowest_level = None

    for left, right in [line.split('~') for line in lines]:
        brick = Brick([int(x) for x in left.split(',')], [int(x) for x in right.split(',')])
        bricks.put(brick)

        if lowest_level is None or brick.floor < lowest_level:
            lowest_level = brick.floor

    return bricks, lowest_level


def do_the_work(lines: list[str]) -> tuple[int, int]:
    bricks, floor_level = parse(lines)
    stable_bricks = []

    while not bricks.empty():
        brick = bricks.get()

        if brick.floor == floor_level:
            stable_bricks.append(brick)
        else:
            support, support_level = [], 0

            for sb in stable_bricks:
                if sb.start[0] <= brick.end[0] and brick.start[0] <= sb.end[0] \
                        and sb.start[1] <= brick.end[1] and brick.start[1] <= sb.end[1]:
                    stable_brick_top = sb.floor + sb.height
                    if stable_brick_top > support_level:
                        support, support_level = [], stable_brick_top
                    if stable_brick_top == support_level:
                        support.append(sb)
            brick.support = support
            brick.floor = support_level + 1
            stable_bricks.append(brick)

            for support_brick in support:
                support_brick.supporting.append(brick)

    count_part1 = 0
    for stable_brick in stable_bricks:
        if len(stable_brick.supporting) > 0:
            if sum([1 for s in stable_brick.supporting if len(s.support) > 1]) == len(stable_brick.supporting):
                count_part1 += 1
        else:
            count_part1 += 1

    count_part2 = 0

    return count_part1, count_part2


def part1(lines: list[str]) -> int:
    res = do_the_work(lines)

    return res[0]


def part2(lines: list[str]) -> int:
    pass


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read().splitlines()

    def test_part1(self):
        res = part1(self.lines)
        self.assertEqual(5, res)

    def test_part2(self):
        res = part2(self.lines)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
