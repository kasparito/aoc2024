package com.hellden.aoc.aoc2024

import com.hellden.collection.cross
import com.hellden.grid.{Grid, Position}

object Day08 extends Day(8):

  private val map = Grid(inputLines)
  import map.Cell

  private def countAntinodePositions(antinodePositions: (Cell, Cell) => Iterable[Position]): Long =
    map
      .cells
      .groupBy(_.value)
      .collect:
        case (value, cells) if value != '.' =>
          cells.cross(cells.toSeq)
      .flatten
      .filterNot(_ == _)
      .toSet
      .flatMap(antinodePositions(_, _))
      .size

  override def part1: Long = // 426
    countAntinodePositions: (cell1, cell2) =>
      val dx = cell1.position.x - cell2.position.x
      val dy = cell1.position.y - cell2.position.y
      (cell1.move(dx, dy).toList ::: cell2.move(-dx, -dy).toList).map(_.position)

  override def part2: Long = // ???
    countAntinodePositions: (cell1, cell2) =>
      val dx = cell1.position.x - cell2.position.x
      val dy = cell1.position.y - cell2.position.y
      (cell1 :: cell1.cellsInDirection(dx, dy).toList ::: cell1.cellsInDirection(-dx, -dy).toList)
        .map(_.position)
