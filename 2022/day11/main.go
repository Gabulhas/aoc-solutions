package main

import (
	"fmt"
)

type monkey struct {
	items       []int
	operation   operation
	testModulo  int
	iftrue      int
	iffalse     int
	inspections int
}

func (m *monkey) toString() {
	itemsString =
		fmt.Sprintf("Items: ")

}

type operation struct {
	aIsOld bool
	a      int

	op func(int, int) int

	bIsOld bool
	b      int
}

func (op operation) executeOperation(v int) int {
	if op.aIsOld {
		op.a = v
	}
	if op.bIsOld {
		op.b = v
	}
	return op.op(op.a, op.b)

}

func (monkey *monkey) giveNewItem(item int) {
	monkey.items = append(monkey.items, item)
}

func runRound(monkeys []*monkey) {
	for _, monkey := range monkeys {
		for _, item := range monkey.items {
			newLevel := monkey.operation.executeOperation(item)
			if newLevel%monkey.testModulo == 0 {
				monkeys[monkey.iftrue].giveNewItem(newLevel)
			} else {
				monkeys[monkey.iffalse].giveNewItem(newLevel)
			}
			monkey.inspections = monkey.inspections + 1
		}

		monkey.items = []int{}
	}
}

func runPartOne(monkeys []*monkey) {
	for i := 0; i < 1000; i++ {
		runRound(monkeys)
	}
	firstBiggest := -1
	secondBiggest := -1
	for _, monkey := range monkeys {
		if monkey.inspections > firstBiggest {
			secondBiggest = firstBiggest
			firstBiggest = monkey.inspections

		} else if monkey.inspections > secondBiggest {
			secondBiggest = monkey.inspections
		}
	}
	fmt.Println("--------------")
	for _, m := range monkeys {
		fmt.Println(m.inspections)

	}
	fmt.Println("--------------")
	fmt.Println(firstBiggest * secondBiggest)

}

func main() {

	monkeys := ReadMonkeysFromStdin()

	for _, monkey := range monkeys {
		fmt.Println(*monkey)
	}
	runPartOne(monkeys)

}
