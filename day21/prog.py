#!/usr/bin/env python3
import sys
import unittest
from collections import deque
from queue import PriorityQueue
from typing import Literal, Tuple

Direction = Literal["N", "S", "E", "W"]


def parse_data(data: str) -> tuple[tuple[str, ...], ...]:
    return tuple(tuple(n for n in line) for line in data.splitlines())


def get_neighbors(
        grid: tuple[tuple[str]],
        pos: tuple[int, int],
) -> tuple[tuple[int, int], ...]:
    neighbors = []
    grid_size = (len(grid), len(grid[0]))

    neighbors.append((pos[0] - 1, pos[1]))
    neighbors.append((pos[0] + 1, pos[1]))
    neighbors.append((pos[0], pos[1] + 1))
    neighbors.append((pos[0], pos[1] - 1))

    return tuple(
        (i, j)
        for i, j in neighbors
        if 0 <= i < grid_size[0] and 0 <= j < grid_size[1] and grid[i][j] == '.'
    )


def find_start(grid: tuple[tuple[str, ...], ...]) -> tuple[int, int]:
    for y in range(len(grid)):
        for x in range(len(grid[y])):
            if grid[y][x] == 'S':
                return x, y


# use Dijkstra's algorithm to find all the spots that are '.' and less than or equal to how_far_away
# from the starting point.
def dijkstra(grid: tuple[tuple[str]], start: tuple[int], how_far_away: int) -> set[tuple[int, int]]:
    visited = set()
    visited_plus_distance = dict()

    queue = PriorityQueue()
    queue.put((0, start))

    while not queue.empty():
        dist, pos = queue.get()
        if pos in visited:
            continue

        visited.add(pos)
        visited_plus_distance[pos] = dist

        if dist > how_far_away:
            continue

        for neighbor in get_neighbors(grid, pos):
            if neighbor not in visited:
                queue.put((dist + 1, neighbor))

    candidates = set()
    for pos, dist in visited_plus_distance.items():
        if dist >= how_far_away or dist == 0:
            candidates.add(pos)

    return candidates


def is_garden_spot(graph, node):
    row, col = node
    return (0 <= row < len(graph)
            and 0 <= col < len(graph[0])
            and graph[row][col] == '.')


def bfs(graph, node, max_steps: int) -> int:  # function for BFS
    visited = []
    queue = deque([(node, 0)])

    while queue:
        (row, col), steps = queue.popleft()

        if steps > max_steps:
            continue

        for neighbor in get_neighbors(graph, (row, col)):
            if is_garden_spot(graph, neighbor) and neighbor not in visited:
                visited.append(neighbor)
                queue.append((neighbor, steps + 1))

    return len([(row, col) for row, col in visited if (row + col) % 2 == max_steps % 2])


def part1(data: str, how_far_away: int) -> int:
    the_map: tuple[tuple[str, ...], ...] = parse_data(data)

    start = find_start(the_map)

    # spots = dijkstra(the_map, start, how_far_away)

    spots = bfs(the_map, start, how_far_away)

    return spots + 1


def part2(lines: list[str]) -> int:
    pass


class TestProg(unittest.TestCase):
    def setUp(self):
        with open('input0.txt') as f:
            self.data = f.read().strip()

    def test_part1(self):
        res = part1(self.data, 6)
        self.assertEqual(16, res)

    def test_part2(self):
        res = part2(self.data)


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        data = f.read().strip()

    print(f'part1 -> {part1(data, 64)}')
    print(f'part2 -> {part2(data)}')
