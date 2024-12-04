package com.hellden.aoc.aoc2024

import com.hellden.grid.Direction.*
import com.hellden.grid.{Direction, Grid}

import scala.language.experimental.namedTuples

object Day04 extends Day(4):

  private val grid = Grid(inputLines)

  override def part1: Long = // 2464
    grid
      .positions
      .zip(Direction.values)
      .count: (position, direction) =>
        "XMAS".zipWithIndex.forall: (char, index) =>
          grid.at(position, direction, index).contains(char)

  override def part2: Long = // 1982
    grid
      .find('A')
      .count: position =>
        def check(direction: Direction): Boolean =
          grid.at(position, direction).contains('M') &&
          grid.at(position, direction.opposite).contains('S')
        (check(NW) || check(SE)) && (check(NE) || check(SW))
