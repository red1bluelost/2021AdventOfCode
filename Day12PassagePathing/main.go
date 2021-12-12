package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
)

type Graph map[string][]string

func ListContains(s string, ls []string) bool {
	for _, l := range ls {
		if l == s {
			return true
		}
	}
	return false
}

func (g *Graph) TotalPaths(start string, seen []string) int {
	if start == "end" {
		return 1
	}
	if start == strings.ToLower(start) {
		if ListContains(start, seen) {
			return 0
		}
		seen = append(seen, start)
	}
	sum := 0
	for _, n := range (*g)[start] {
		sum += g.TotalPaths(n, seen)
	}
	return sum
}

func (g *Graph) TotalPaths2(
	start string,
	seenOnce []string,
	seenTwiceStr string,
) int {
	if start == "end" {
		return 1
	}
	if start == strings.ToLower(start) {
		seen := ListContains(start, seenOnce)
		if seen && (seenTwiceStr != "" || start == "start") {
			return 0
		}
		if seen && seenTwiceStr == "" {
			seenTwiceStr = start
		}
		seenOnce = append(seenOnce, start)
	}
	sum := 0
	for _, n := range (*g)[start] {
		sum += g.TotalPaths2(n, seenOnce, seenTwiceStr)
	}
	return sum
}

func (g *Graph) String() string {
	s := ""
	for k, v := range *g {
		s += fmt.Sprintf("[%s] => ", k)
		for i, n := range v {
			if i == 0 {
				s += fmt.Sprintf("%s", n)
			} else {
				s += fmt.Sprintf(" -> %s", n)
			}
		}
		s += fmt.Sprintf("\n")
	}
	return s
}

func main() {
	var f io.Reader
	if len(os.Args) > 1 {
		var err error
		if f, err = os.Open(os.Args[1]); err != nil {
			panic("Bad file")
		}
	} else {
		f = os.Stdin
	}

	scanner := bufio.NewScanner(f)

	graph := make(Graph)
	for scanner.Scan() {
		pair := strings.Split(scanner.Text(), "-")
		to, from := pair[0], pair[1]
		if _, ok := graph[to]; !ok {
			graph[to] = make([]string, 0)
		}
		graph[to] = append(graph[to], from)
		if _, ok := graph[from]; !ok {
			graph[from] = make([]string, 0)
		}
		graph[from] = append(graph[from], to)
	}

	fmt.Printf(
		"Part 1 = %d\n",
		graph.TotalPaths("start", make([]string, 0)),
	)
	fmt.Printf(
		"Part 2 = %d\n",
		graph.TotalPaths2("start", make([]string, 0), ""),
	)

	fmt.Printf("\nGraph\n%s", graph.String())
}
