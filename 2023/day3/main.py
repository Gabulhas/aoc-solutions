from sys import stdin


def read_input():
    return [line for line in stdin]


def is_symbol(ch):
    return not (ch.isdigit() or ch == '.' or ch.isspace())


def get_adjacent_positions(x, y, max_x, max_y):
    return [(x + dx, y + dy) for dx in [-1, 0, 1] for dy in [-1, 0, 1] if (0 <= x + dx < max_x and 0 <= y + dy < max_y) and not (dx == 0 and dy == 0)]


def is_any_symbol_around(schematic, positions):
    checked_positions = set()
    for x, y in positions:
        if (x, y) in checked_positions:
            continue
        checked_positions.add((x, y))
        for adj_x, adj_y in get_adjacent_positions(x, y, len(schematic[0]), len(schematic)):
            if (adj_x, adj_y) not in checked_positions and is_symbol(schematic[adj_y][adj_x]):
                return True
    return False


def part_one_thing(schematic):
    total_sum = 0

    def add_part_number_if_valid(number, positions):
        nonlocal total_sum
        if number and is_any_symbol_around(schematic, positions):
            total_sum += int(number)

    for y, line in enumerate(schematic):
        current_number = ''
        number_positions = []
        for x, ch in enumerate(line):
            if ch.isdigit():
                current_number += ch
                number_positions.append((x, y))
            else:
                add_part_number_if_valid(current_number, number_positions)
                current_number = ''
                number_positions = []

        # Handle number at the end of the line
        add_part_number_if_valid(current_number, number_positions)

    return total_sum


def is_any_stars_around(schematic, positions):
    checked_positions = set()
    for x, y in positions:
        if (x, y) in checked_positions:
            continue
        checked_positions.add((x, y))
        for adj_x, adj_y in get_adjacent_positions(x, y, len(schematic[0]), len(schematic)):
            if (adj_x, adj_y) not in checked_positions and schematic[adj_y][adj_x] == "*":
                return (adj_x, adj_y)
    return None


def part_two_thing(schematic):

    found_stars = {}

    def add_part_number_if_gear(number, positions):
        nonlocal found_stars
        possible_star_location = is_any_stars_around(schematic, positions)
        if number and possible_star_location:
            if possible_star_location in found_stars:
                found_stars[possible_star_location].append(int(number))
            else:
                found_stars[possible_star_location] = [int(number)]

    for y, line in enumerate(schematic):
        current_number = ''
        number_positions = []
        for x, ch in enumerate(line):
            if ch.isdigit():
                current_number += ch
                number_positions.append((x, y))
            else:
                add_part_number_if_gear(current_number, number_positions)
                current_number = ''
                number_positions = []
        add_part_number_if_gear(current_number, number_positions)

    total_gear_sum = 0

    for v in found_stars.values():
        if len(v) == 2:
            total_gear_sum += v[0] * v[1]

    return total_gear_sum


def main():
    schematic = read_input()
    the_something = part_one_thing(schematic)
    the_something_two = part_two_thing(schematic)
    print(the_something)
    print(the_something_two)


if __name__ == "__main__":
    main()
