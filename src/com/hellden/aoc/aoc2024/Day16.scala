package com.hellden.aoc.aoc2024

import com.hellden.grid.*
import com.hellden.grid.Direction.*
import com.hellden.grid.PathFinder.*

object Day16 extends Day(16):

  type Num = Long

  private val maze = Grid(inputLines)

  private val start = maze.find('S').head.position
  private val finish = maze.find('E').head.position

  private val bestSolutions =

    val pathFinder = new PathFinder(maze):
      override def cost(direction: Direction, turn: Turn): Num = 1000
      override def key(path: Path): Any = (path.position, path.direction)

    val sortedSolutions = pathFinder.find(E, start, finish).iterable.toSeq.sortBy(_.score)
    sortedSolutions.takeWhile(_.score == sortedSolutions.head.score)

  override def part1: Num = // 105508
    bestSolutions.head.score

  override def part2: Num = // 548
    bestSolutions.flatMap(_.trail).distinct.size
