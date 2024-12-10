package com.hellden.aoc.aoc2024

import com.hellden.grid.{Direction, Grid, Position}

import scala.annotation.tailrec

object Day10 extends Day(10):

  private val map = Grid(inputLines)
  import map.Cell

  private val directions = Direction.values.filter(_.ordinal % 2 == 0)

  @tailrec
  private def reachable(cells: List[Cell], reached: List[Position] = Nil): List[Position] =
    cells match
      case Nil =>
        reached
      case Cell(position, '9') :: tail =>
        reachable(tail, position :: reached)
      case cell :: tail =>
        reachable(directions.flatMap(cell.move(_)).filter(_.value == cell.value + 1).toList ::: tail, reached)

  private val trailHeads = map.find('0').toList

  override def part1: Long = // 698
    trailHeads.map(head => reachable(List(head)).toSet.size).sum

  override def part2: Long = // 1436
    reachable(trailHeads).size
