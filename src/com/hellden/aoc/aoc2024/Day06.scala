package com.hellden.aoc.aoc2024

import com.hellden.grid.Direction.*
import com.hellden.grid.Direction.Turn.*
import com.hellden.grid.{Direction, Grid, Position}

import scala.annotation.tailrec
import scala.language.experimental.namedTuples

object Day06 extends Day(6):

  private val grid = Grid(inputLines)

  private def countVisitedCells(obstacle: Option[grid.Cell] = None): (visitedCells: Set[grid.Cell], loop: Boolean) =

    def isObstacle(cell: grid.Cell): Boolean =
      obstacle.contains(cell) || cell.value != '.' && cell.value != '^'

    @tailrec
    def helper(
      cell: grid.Cell,
      direction: Direction,
      visitedCells: Set[grid.Cell],
      positionDirections: Set[(Position, Direction)]
    ): (visitedCells: Set[grid.Cell], loop: Boolean) =
      cell.move(direction) match
        case None =>
          (visitedCells = visitedCells, loop = false)
        case Some(next) if positionDirections.contains(next.position -> direction) =>
          (visitedCells = visitedCells, loop = true)
        case Some(next) if isObstacle(next) =>
          val nextDirection = direction.turn(Right)
          helper(cell, nextDirection, visitedCells, positionDirections + (cell.position -> nextDirection))
        case Some(next) =>
          helper(next, direction, visitedCells + next, positionDirections + (next.position -> direction))

    val start = grid.find('^').head
    helper(start, N, Set(start), Set(start.position -> N))

  private val initial = countVisitedCells()

  override def part1: Int = // 5067
    initial.visitedCells.size

  override def part2: Long = // ???
    initial.visitedCells.filterNot(_.value == '^').count: obstacleCell =>
      countVisitedCells(Some(obstacleCell)).loop
