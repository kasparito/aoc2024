package com.hellden.aoc.aoc2024

import com.hellden.grid.Direction.*
import com.hellden.grid.{Direction, Grid, Position}

import scala.language.experimental.namedTuples

object Day04 extends Day(4):

  private val grid = Grid(inputLines)

  override def part1: Long = // 2464
    grid
      .positions
      .map: position =>
        Direction.values.count: direction =>
          "XMAS"
            .zipWithIndex
            .forall: (char, index) =>
              grid.valueFor(position.move(direction, index)).contains(char)
      .sum

  override def part2: Long = // 1982
    grid
      .find('A')
      .count: position =>
        def check(d: Direction): Boolean =
          grid.valueFor(position.move(d)).contains('M') &&
          grid.valueFor(position.move(d.opposite)).contains('S')
        (check(NW) || check(SE)) && (check(NE) || check(SW))
