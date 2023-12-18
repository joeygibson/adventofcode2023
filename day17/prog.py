#!/usr/bin/env python3
import enum
import math
import sys
import unittest
from collections import defaultdict
from queue import PriorityQueue
from typing import Literal

Direction = Literal["N", "S", "E", "W"]
Point = tuple[int, int, Direction | None]


def parse_data(data: str) -> tuple[tuple[int]]:
    return tuple(tuple(int(n) for n in line) for line in data.splitlines())


def get_neighbors(
        grid: tuple[tuple[int]],
        pos: tuple[int, int],
        direction: Direction | None = None,
        min_dist: int = 1,
        max_dist: int | None = None,
) -> tuple[Point]:
    """Return the neighbors that can be travelled to.

    :param grid: The grid map.
    :param pos: The current position.
    :param direction: The last direction travelled.
    :param min_dist: The minimum distance that must be travelled.
    :param max_dist: The maximum distance that can be travelled.
    :returns: A tuple of points that can be reached from this node.
    """
    neighbors = []
    grid_size = (len(grid), len(grid[0]))
    for d in range(min_dist, (max_dist + 1) or max(grid_size)):
        if direction is None or direction in "EW":
            neighbors.append((pos[0] - d, pos[1], "N"))
            neighbors.append((pos[0] + d, pos[1], "S"))
        if direction is None or direction in "NS":
            neighbors.append((pos[0], pos[1] + d, "E"))
            neighbors.append((pos[0], pos[1] - d, "W"))

    return tuple(
        (i, j, d)
        for i, j, d in neighbors
        if 0 <= i < grid_size[0] and 0 <= j < grid_size[1]
    )


def a_star(
        grid: tuple[tuple[int]], min_dist: int = 1, max_dist: int | None = None
) -> int:
    """Perform the A* search algorithm on the grid map.

    This finds the cheapest path from the starting point `(0,0)` to the exit
    `(len(grid)-1, len(grid[0])-1)`.

    :param grid: The grid map.
    :param min_dist: The minimum distance that must be travelled in a given
        direction.
    :param max_dist: The maximum distance that can be travelled in a given
        direction.
    :returns: The score of the cheapest path through the grid.
    :raises ValueError: If a path cannot be found through the grid.
    """
    goal = (len(grid) - 1, len(grid[0]) - 1)

    # The queue of nodes to be checked, ordered by their current cheapest cost
    # from the starting node. Cheapest entries are retrieved first.
    unsolved = PriorityQueue()
    unsolved.put((0, (0, 0, None)))

    # A map of points (and the direction they are reached from) to their score.
    # Need to store the direction travelled to reach them as well, as different
    # paths may pass through the same node in different directions, yielding
    # different paths.
    scores = defaultdict(lambda: math.inf)
    scores[(0, 0, None)] = 0

    while unsolved:
        score, closest_point = unsolved.get()
        current, direction = closest_point[:2], closest_point[2]
        if current == goal:
            return score

        for n in get_neighbors(grid, current, direction, min_dist, max_dist):
            cost = 0
            if n[2] == "N":
                cost = sum(grid[i][n[1]] for i in range(n[0], current[0]))
            elif n[2] == "S":
                cost = sum(grid[i][n[1]] for i in range(current[0] + 1, n[0] + 1))
            elif n[2] == "E":
                cost = sum(grid[n[0]][current[1] + 1 : n[1] + 1])
            elif n[2] == "W":
                cost = sum(grid[n[0]][n[1] : current[1]])

            n_score = score + cost
            if n_score < scores[n]:
                scores[n] = n_score
                unsolved.put((n_score, n))

    raise ValueError("No path through the grid.")


def part1(data: str) -> int:
    graph = parse_data(data)
    return a_star(graph, 1, 3)


def part2(data: str) -> int:
    graph = parse_data(data)
    return a_star(graph, 4, 10)


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.lines = f.read()

    def test_part1(self):
        res = part1(self.lines)
        self.assertEqual(102, res)

    def test_part2(self):
        res = part2(self.lines)
        self.assertEqual(71, res)

if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
