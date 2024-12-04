package com.hellden.aoc.aoc2024

import com.hellden.collection.*
import com.hellden.grid.Direction.*
import com.hellden.grid.{Direction, Grid}

import scala.language.experimental.namedTuples

object Day04 extends Day(4):

  private val grid = Grid(inputLines)

  override def part1: Long = // 2464
    grid.cells.cross(Direction.values).count: (cell, direction) =>
      cell.valuesIn(direction).startsWith("XMAS")

  override def part2: Long = // 1982
    grid.cells.count: cell =>
      Diagonal.values.forall: diagonal =>
        diagonal.directions.exists: direction =>
          cell.valuesIn(direction, offset = -1).startsWith("MAS")
