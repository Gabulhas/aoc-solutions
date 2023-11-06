package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

type Pair struct {
	x int
	y int
}

var down = []Pair{{x: 0, y: 1}}
var downAndLeft = []Pair{{x: 0, y: 1}, {x: -1, y: 0}}
var downAndRight = []Pair{{x: 0, y: 1}, {x: 1, y: 0}}
var possibleMoves = [][]Pair{down, downAndLeft, downAndRight}

func addPairs(a Pair, b Pair) Pair {
	return Pair{
		x: a.x + b.x,
		y: a.y + b.y,
	}
}

func (p Pair) toString() string {
	return fmt.Sprintf("%d,%d", p.x, p.y)
}

type MapInfo struct {
	pointsFilled map[string]bool
	lowestY      int
	rightmostX   int
	leftmostX    int
}

func (m MapInfo) print() {

	fmt.Printf("lowestY %d\nrightmostX %d\nleftmostX %d\n", m.lowestY, m.rightmostX, m.leftmostX)

	for y := 0; y <= m.lowestY; y++ {
		for x := m.leftmostX; x <= m.rightmostX; x++ {
			if isWall, filled := m.pointsFilled[Pair{x, y}.toString()]; filled {
				if isWall {
					fmt.Printf("#")
				} else {
					fmt.Printf("O")
				}
			} else {
				fmt.Printf(".")
			}
		}
		fmt.Println()
	}
}

func inputLineToPairSlice(line string) []Pair {
	var result []Pair

	for _, point := range strings.Split(line, " -> ") {
		coords := strings.Split(point, ",")
		if len(coords) != 2 {
			log.Fatalf("Failed while parsing %s", point)
		} else {
			x, errx := strconv.Atoi(coords[0])
			y, erry := strconv.Atoi(coords[1])

			if errx != nil || erry != nil {
				log.Fatalf("Failed while parsing %s", point)
			} else {
				result = append(result, Pair{x, y})
			}
		}

	}
	return result

}

func addEverySinglePoint(mapInfo *MapInfo, start Pair, end Pair) {
	var change Pair
	if start.x == end.x {
		if start.y > end.y {
			change = Pair{x: 0, y: -1}
		} else {
			change = Pair{x: 0, y: 1}
		}
	} else {
		if start.x > end.x {
			change = Pair{x: -1, y: 0}
		} else {
			change = Pair{x: 1, y: 0}
		}
	}

	fmt.Println("----------------------------------------------------------------")
	fmt.Printf("Start %s | End %s | Change %s\n", start.toString(), end.toString(), change.toString())

	current := start

	// since they are straight lines, either x or y will always be the same
	for true {
		fmt.Printf("Adding %s\n", current.toString())
		if mapInfo.rightmostX < current.x {
			mapInfo.rightmostX = current.x
		} else if mapInfo.leftmostX > current.x {
			mapInfo.leftmostX = current.x
		}
		if mapInfo.lowestY < current.y {
			mapInfo.lowestY = current.y
		}

		mapInfo.pointsFilled[current.toString()] = true
		current = addPairs(current, change)
		if current.x == end.x && current.y == end.y {
			break
		}
	}

}

func addInputLineToMap(mapInfo *MapInfo, line string) {
	pairs := inputLineToPairSlice(line)

	for i := 1; i < len(pairs); i++ {
		addEverySinglePoint(mapInfo, pairs[i-1], pairs[i])
	}

}

func newMapInfo() *MapInfo {
	return &MapInfo{
		pointsFilled: make(map[string]bool),
		lowestY:      -math.MaxInt64,
		rightmostX:   -math.MaxInt64,
		leftmostX:    math.MaxInt64,
	}
}

type MoveResult int64

const (
	Moved MoveResult = iota
	FellOf
	NotPossible
)

func calculateMove(mapInfo *MapInfo, currentPosition Pair, move []Pair) (result MoveResult, nextPos Pair) {
	for _, step := range move {
		nextPos = addPairs(currentPosition, step)
		fmt.Printf("Next step %s\n", nextPos.toString())

		if _, alreadyFilled := mapInfo.pointsFilled[nextPos.toString()]; alreadyFilled {
			result = NotPossible
			return
		} else if nextPos.y >= mapInfo.lowestY {
			result = FellOf
			return
		} else {
			result = Moved
		}

	}
	return
}

func fallSand(mapInfo *MapInfo) bool {
	currentPosition := Pair{x: 500, y: 0}

	for true {

		didMove := false

		for _, possibleMove := range possibleMoves {

			result, nextPosition := calculateMove(mapInfo, currentPosition, possibleMove)
			fmt.Printf("Next position %s\n", nextPosition.toString())
			if result == Moved {
				currentPosition = nextPosition
				didMove = true
				break
			} else if result == FellOf {
				return false
			}

		}
		if !didMove {
			mapInfo.pointsFilled[currentPosition.toString()] = false
			return true
		}
	}

	return true

}

func countSand(mapInfo *MapInfo) int {
	count := 0
	mapInfo.print()
	for fallSand(mapInfo) {
		mapInfo.print()
		count++
	}
	mapInfo.print()
	return count
}

func main() {

	mapInfo := newMapInfo()

	scanner := bufio.NewScanner(os.Stdin)
	// optionally, resize scanner's capacity for lines over 64K, see next example
	for scanner.Scan() {
		addInputLineToMap(mapInfo, scanner.Text())
	}

	for key := range mapInfo.pointsFilled {
		fmt.Printf("Point %s\n", key)
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)

	}

	mapInfo.print()
	fmt.Println(countSand(mapInfo))

}
