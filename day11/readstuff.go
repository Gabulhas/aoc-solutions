package main

import (
	"bufio"
	"log"
	"os"
	"strconv"
	"strings"
)

func stringToOp(s string) func(int, int) int {
	if s == "+" {
		return func(i1, i2 int) int { return i1 + i2 }
	} else if s == "*" {
		return func(i1, i2 int) int { return i1 * i2 }
	}

	log.Fatal("Impossble op", s)
	return func(i1, i2 int) int { return 0 }
}

func readItemLine(line string) []int {
	words := strings.Split(line, " ")
	var items []int
	if len(words) < 3 {
		log.Fatal("Not enough items", line)
	}
	for _, word := range words[2:] {
		word = strings.TrimRight(word, ",")
		if asInt, err := strconv.Atoi(word); err == nil {
			items = append(items, asInt)
		} else {
			log.Fatal(err)
		}
	}
	return items
}

func readOperationLine(line string) operation {
	var ope operation
	words := strings.Split(line, " ")
	if len(words) < 6 {
		log.Fatal("Not enough words", line)
	}

	if words[3] == "old" {
		ope.aIsOld = true
	} else if asInt, err := strconv.Atoi(words[3]); err == nil {
		ope.aIsOld = false
		ope.a = asInt
	} else {
		log.Fatal(err)
	}

	if words[5] == "old" {
		ope.bIsOld = true
	} else if asInt, err := strconv.Atoi(words[5]); err == nil {
		ope.bIsOld = false
		ope.b = asInt
	} else {
		log.Fatal(err)
	}
	ope.op = stringToOp(words[4])
	return ope

}

func readLastIntFromLine(line string, requiredWords int) int {
	words := strings.Split(line, " ")
	if len(words) != requiredWords {
		log.Fatal("Not enough words", line)
	}
	if asInt, err := strconv.Atoi(words[len(words)-1]); err == nil {
		return asInt
	} else {
		log.Fatal(err)
		return 0
	}
}

func shouldReadLine(scanner *bufio.Scanner) string {
	if scanner.Scan() {
		return strings.TrimSpace(scanner.Text())
	} else {
		log.Fatal("Failed to read line")
	}
	return ""
}

func readMonkey(scanner *bufio.Scanner) *monkey {
	firstText := scanner.Text()
	if firstText == "\n" || firstText == "" {
		scanner.Scan()
	}
	nextLineYield := func() string { return shouldReadLine(scanner) }
	return &monkey{
		items:       readItemLine(nextLineYield()),
		operation:   readOperationLine(nextLineYield()),
		testModulo:  readLastIntFromLine(nextLineYield(), 4),
		iftrue:      readLastIntFromLine(nextLineYield(), 6),
		iffalse:     readLastIntFromLine(nextLineYield(), 6),
		inspections: 0,
	}

}

func ReadMonkeysFromStdin() []*monkey {
	var monkeys []*monkey
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		monkeys = append(monkeys, readMonkey(scanner))
	}
	if scanner.Err() != nil {
		log.Fatal(scanner.Err())
	}
	return monkeys
}
