package main

import (
	"fmt"
	"io"
	"math"
	"os"
)

func Signum(i int) int {
	switch {
	case i < 0:
		return -1
	case i > 0:
		return 1
	default:
		return 0
	}
}

type Posn struct {
	X int
	Y int
}

type Velocity Posn

func (v Velocity) Step() Velocity {
	return Velocity{v.X - Signum(v.X), v.Y - 1}
}

type TargetArea struct {
	BL Posn
	TR Posn
}

func (ta TargetArea) IsOvershotX(posn Posn) bool {
	return posn.X > ta.TR.X
}
func (ta TargetArea) IsOvershotY(posn Posn) bool {
	return posn.Y < ta.BL.Y
}
func (ta TargetArea) IsOvershot(posn Posn) bool {
	return ta.IsOvershotX(posn) || ta.IsOvershotY(posn)
}

func (ta TargetArea) IsUndershootX(posn Posn) bool {
	return posn.X < ta.BL.X
}

func (ta TargetArea) IsHitX(posn Posn) bool {
	return posn.X <= ta.TR.X && posn.X >= ta.BL.X
}
func (ta TargetArea) IsHitY(posn Posn) bool {
	return posn.Y <= ta.TR.Y && posn.Y >= ta.BL.Y
}
func (ta TargetArea) IsHit(posn Posn) bool {
	return ta.IsHitX(posn) && ta.IsHitY(posn)
}

func SimulateJustX(area TargetArea, velocity Velocity) []Posn {
	posns := make([]Posn, 1)
	for {
		last := posns[len(posns)-1]
		if area.IsHitX(last) {
			return posns
		} else if area.IsOvershotX(last) {
			return nil
		} else if velocity.X == 0 && area.IsUndershootX(last) {
			return nil
		}
		posns = append(posns, Posn{
			last.X + velocity.X,
			0,
		})
		velocity = velocity.Step()
	}
}
func Simulate(area TargetArea, velocity Velocity) []Posn {
	posns := make([]Posn, 1)
	for {
		last := posns[len(posns)-1]
		if area.IsHit(last) {
			return posns
		} else if area.IsOvershot(last) {
			return nil
		} else if velocity.X == 0 && area.IsUndershootX(last) {
			return nil
		}
		posns = append(posns, Posn{
			last.X + velocity.X,
			last.Y + velocity.Y,
		})
		velocity = velocity.Step()
	}
}

func FindMinimumXVelocity(area TargetArea) int {
	var v Velocity
	for {
		if s := SimulateJustX(area, v); s != nil {
			return v.X
		}
		v.X++
	}
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

	var ta TargetArea
	if n, err := fmt.Fscanf(
		f, "target area: x=%d..%d, y=%d..%d",
		&ta.BL.X, &ta.TR.X, &ta.BL.Y, &ta.TR.Y,
	); n != 4 || err != nil {
		panic("bad input")
	}

	minXVel := FindMinimumXVelocity(ta)
	var v Velocity
	var highestVerticalY Velocity
	var highestSim []Posn
	for v.X, v.Y = minXVel, 0; v.Y < 1000; v.Y = v.Y + 1 {
		if s := Simulate(ta, v); s != nil {
			highestSim = s
			highestVerticalY = v
		}
	}
	maxHeight := func() int {
		max := math.MinInt32
		for _, posn := range highestSim {
			if posn.Y > max {
				max = posn.Y
			}
		}
		return max
	}()
	fmt.Printf("Part 1 = %d\n", maxHeight)

	count := 0
	for x := minXVel; x <= ta.TR.X; x++ {
		for y := ta.BL.Y; y <= highestVerticalY.Y; y++ {
			if s := Simulate(ta, Velocity{x, y}); s != nil {
				count++
			}
		}
	}
	fmt.Printf("Part 2 = %d\n", count)
}
