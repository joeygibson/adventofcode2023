#!/usr/bin/env python3
import re
import sys

MAX_RED = 12
MAX_GREEN = 13
MAX_BLUE = 14


def part1(lines: list[str]) -> int:
    games = {}

    for line in lines:
        chunks = line.split(':')
        game_id = int(chunks[0].removeprefix('Game '))
        results = chunks[1].split('; ')

        game = games.get(game_id, {})

        results = re.findall(r'(\d+ \w+)', line)

        for r in results:
            num, color = r.strip().split(' ')
            num = int(num)

            new_value = max(game.get(color, 0), num)
            game[color] = new_value

            games[game_id] = game

    print(games)

    possible_games = [k for k, v in games.items() if
                      v['red'] <= MAX_RED and v['green'] <= MAX_GREEN and v['blue'] <= MAX_BLUE]

    return sum(possible_games)


def part2(lines: list[str]) -> int:
    pass


if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        lines = f.read().splitlines()

    print(f'part1 -> {part1(lines)}')
    print(f'part2 -> {part2(lines)}')
