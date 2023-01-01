from sys import stdin


def read_input():
    return [line for line in stdin]


def parse_line(line):
    return tuple(line.split(" "))


def direction_to_tuple(direction):
    if direction == "U":
        return (0, 1)
    elif direction == "D":
        return (0, -1)
    elif direction == "R":
        return (1, 0)
    elif direction == "L":
        return (-1, 0)


def new_pos(current, dir):
    x, y = current
    x2, y2 = direction_to_tuple(dir)
    return (x + x2, y + y2)


def distance(a, b):
    x, y = a
    x2, y2 = b
    return (x - x2, y - y2)


def is_tail_touching(head, tail):
    (xdis, ydis) = distance(head, tail)
    if abs(xdis) > 1 or abs(ydis) > 1:
        return False
    return True


def new_tail_pos(head, tail):
    tailx, taily = tail
    xdis, ydis = distance(head, tail)

    if abs(xdis) <= 1 and abs(ydis) <= 1:
        return tail

    elif xdis == 0 and abs(ydis) > 1:
        return (tailx, taily + (ydis / 2))

    elif abs(xdis) > 1 and ydis == 0:
        return (tailx + (xdis / 2), taily)

    elif (abs(xdis) > 1 and abs(ydis) >= 1) or (abs(xdis) >= 1 and abs(ydis) > 1):
        return (tailx + xdis/abs(xdis), taily + ydis/abs(ydis))

    else:
        raise Exception(
            f"Impossible distance: {(xdis, ydis)}. Head: {head}, Tail: {tail}")


def do_the_walking(lines):
    tail_knots = [(0,0)] * 10
    all_tail_pos = set()

    for direction, amount in [parse_line(l) for l in lines]:
        for step in range(int(amount)):
            tail_knots[0] = new_pos(tail_knots[0], direction)
            for knot_index in range(len(tail_knots) - 1 , 0, -1):
                tail_knots[knot_index] = new_tail_pos(tail_knots[knot_index - 1], tail_knots[knot_index])
            all_tail_pos.add(tail_knots[len(tail_knots) - 1])


    print(all_tail_pos)
    print(len(all_tail_pos))


if __name__ == "__main__":
    do_the_walking(read_input())
