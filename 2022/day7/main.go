package main

import (
	"fmt"
	"io"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

type directory struct {
	parent      *directory
	name        string
	directories map[string]*directory
	files       map[string]*file
	totalSize   int
}

func newDirectory(name string, parent *directory) *directory {
	var newDire *directory
	newDire = new(directory)
	newDire.name = name
	newDire.parent = parent
	newDire.directories = make(map[string]*directory)
	newDire.files = make(map[string]*file)

	return newDire

}

type file struct {
	name string
	size int
}

func depthToSpaces(depth int) string {
	result := ""
	for i := 0; i < depth; i++ {
		result = result + "  "
	}
	return result

}

func printDirectory(currentDirectory *directory, depth int) {
	fmt.Printf("\n%s- %s (dir) (size=%d)", depthToSpaces(depth), currentDirectory.name, currentDirectory.totalSize)
}

func printFile(f *file, depth int) {
	fmt.Printf("\n%s- %s (file, size=%d)", depthToSpaces(depth), f.name, f.size)
}

func printDirectoryTree(currentDirectory *directory, depth int) {
	printDirectory(currentDirectory, depth)
	for _, directory := range currentDirectory.directories {
		printDirectoryTree(directory, depth+1)
	}
	for _, f := range currentDirectory.files {
		printFile(f, depth+1)
	}

}

func getResponse(currentDirectory *directory, minimum int) (int, *directory) {
	localMinimum := math.MaxInt32
	var directoryMinimum *directory

	if currentDirectory.totalSize >= minimum {
		localMinimum = currentDirectory.totalSize
		directoryMinimum = currentDirectory
	}
	for _, d := range currentDirectory.directories {
		tempMinimum, temDMinimum := getResponse(d, minimum)
		if tempMinimum < localMinimum {
			localMinimum = tempMinimum
			directoryMinimum = temDMinimum
		}

	}
	return localMinimum, directoryMinimum
}

func calculateSize(dir *directory) int {
	totalSize := 0
	for _, f := range dir.files {
		totalSize += f.size
	}

	for _, d := range dir.directories {
		totalSize += calculateSize(d)
	}

	dir.totalSize = totalSize
	return totalSize
}

func doStuff(lines *[]string) {
	rootDir := newDirectory("/", nil)
	currentDirectory := rootDir

	for _, line := range *lines {
		lineArgs := strings.Split(line, " ")
		if len(lineArgs) < 2 {
			continue
		}

		switch {
		case lineArgs[0] == "$": //command
			if lineArgs[1] == "cd" {
				if lineArgs[2] == ".." {
					currentDirectory = currentDirectory.parent
				} else if lineArgs[2] == "/" {
					currentDirectory = rootDir
				} else {
					currentDirectory = currentDirectory.directories[lineArgs[2]]
				}

			} else if lineArgs[1] == "ls" {
				continue
			}

		case lineArgs[0] == "dir": //directory
			currentDirectory.directories[lineArgs[1]] = newDirectory(lineArgs[1], currentDirectory)

		default: //file
			name := lineArgs[1]
			size, err := strconv.Atoi(lineArgs[0])
			if err != nil {
				log.Fatal(err)
			} else {
				currentDirectory.files[name] = &file{name: name, size: size}
			}

		}

	}
	calculateSize(rootDir)
	printDirectoryTree(rootDir, 0)
	leftToGo := 30000000 - (70000000 - rootDir.totalSize)
	fmt.Printf("\nSpace that i need to delete %d", leftToGo)

	minimumStorage, directoryToDelete := getResponse(rootDir, leftToGo)
	fmt.Printf("Directory: %s | Space to delete %d", directoryToDelete.name, minimumStorage)

}

func main() {

	stdin, err := io.ReadAll(os.Stdin)
	if err != nil {
		log.Fatal(err)
	}
	stdinAsString := string(stdin)
	lines := strings.Split(stdinAsString, "\n")[1:] // I use [1:] because the first line is always $ cd / I guess
	doStuff(&lines)

}
