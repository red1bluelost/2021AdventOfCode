package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strings"
)

type BingoTable struct {
	board  [5][5]int
	marked [5][5]bool
	hasWon bool
}

func NewBingoTable(t [5][5]int) *BingoTable {
	return &BingoTable{board: t}
}

func (bt *BingoTable) Mark(n int) (int, int) {
	for r, row := range bt.board {
		for c, num := range row {
			if num == n {
				bt.marked[r][c] = true
				return r, c
			}
		}
	}
	return -1, -1
}

func (bt *BingoTable) CheckWin(r, c int) bool {
	row, col := true, true
	for i := range bt.marked {
		row = row && bt.marked[r][i]
		col = col && bt.marked[i][c]
	}
	bt.hasWon = row || col
	return bt.hasWon
}

func (bt *BingoTable) CalculateWin(wr, wc int) int {
	sum := 0
	for r, row := range bt.board {
		for c, num := range row {
			if !bt.marked[r][c] {
				sum += num
			}
		}
	}
	return sum * bt.board[wr][wc]
}

func (bt *BingoTable) HasWon() bool {
	return bt.hasWon
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

	if !scanner.Scan() {
		panic("Too early on first line failed")
	}

	draws := make([]int, 0)
	flr := bufio.NewReader(strings.NewReader(scanner.Text()))
	for {
		s, err := flr.ReadString(',')
		if err == io.EOF {
			break
		}
		var i int
		if n, err := fmt.Sscanf(s, "%d,", &i); n != 1 || err != nil {
			panic("Bad read of draw")
		}
		draws = append(draws, i)
	}

	tables := make([]*BingoTable, 0)
	for {
		var board [5][5]int
		if !scanner.Scan() { // Discard empty line
			break
		}
		for i := 0; i < 5; i++ {
			scanner.Scan()
			n, err := fmt.Sscanf(scanner.Text(), "%2d %2d %2d %2d %2d",
				&board[i][0], &board[i][1], &board[i][2],
				&board[i][3], &board[i][4],
			)
			if n != 5 || err != nil {
				panic("Bad read of board row")
			}
		}
		tables = append(tables, NewBingoTable(board))
	}

	i := 0
	for d, draw := range draws {
		for t, table := range tables {
			if table.HasWon() {
				continue
			}
			if r, c := table.Mark(draw); r != -1 && c != -1 {
				if table.CheckWin(r, c) {
					win := table.CalculateWin(r, c)
					fmt.Printf("Winner %d\n", i)
					fmt.Printf("Win = %d\n", win)
					fmt.Printf("Draw = (%d) %d\n", d, draw)
					fmt.Printf("Table num = (%d)\n\n", t)
					i++
				}
			}
		}
	}
}
