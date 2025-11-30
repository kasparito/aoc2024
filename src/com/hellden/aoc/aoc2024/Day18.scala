package com.hellden.aoc.aoc2024

import com.hellden.grid.*
import com.hellden.grid.Direction.*
import com.hellden.grid.PathFinder.*

import scala.annotation.tailrec

object Day18 extends Day(18):

  type Num = Long

  val dimension: Int = 70

  val start: Position = Position(0, 0)
  val finish: Position = Position(dimension, dimension)

  val bytePositions: IndexedSeq[Position] = inputLines.map(Position.parse)

  class Computer(bytes: Int):
  
    val grid: BoundedGrid[Char] =
      val corrupted = bytePositions.take(bytes).toSet
      val verticalBounds = 0 to dimension
      val horisontalBounds = 0 to dimension
      BoundedGrid[Char](horisontalBounds, verticalBounds): (x, y) =>
        val position = Position(x, y)
        Option.when(horisontalBounds.contains(position.x) && verticalBounds.contains(position.y)):
          if corrupted.contains(position) then
            '#'
          else
            '.'
  
    def find(direction: Direction, start: Position, finish: Position): Solution =
      val pathFinder = new PathFinder(grid):
        override def heuristicScore(path: Path): Num =
          -path.score - path.position.manhattanDistance(finish)
      pathFinder.find(direction, start, finish).head.toSolution

  override def part1: Num = // 340
    val middle = Position(dimension / 2, dimension / 2)
    val computer = Computer(1024)
    val forward = computer.find(E, start, middle).score
    val backward = computer.find(E.opposite, finish, middle).score
    forward + backward

  override def part2: String = // ???

    val middle = Position(dimension / 2, dimension / 2)
    val bottomLeft = Position(0, dimension)

    @tailrec
    def helper(bytes: Int, position: Position): String =
      println(s"$position: $bytes -> ${bytePositions(bytes)}")
      val computer = Computer(bytes)
      val forward = computer.find(E, start, position).trail
      val backward = computer.find(E.opposite, finish, position).trail
      val trail = forward.toSet ++ backward
      val nextBytes = LazyList
        .from(bytes)
        .find: index =>
          trail.contains(bytePositions(index))
        .get
      if position == bottomLeft then
        val Position(x, y) = bytePositions(nextBytes) 
        s"$x,$y"
      else if nextBytes != bytes then
        helper(nextBytes, position)
      else 
        helper(nextBytes, position.moveIn(SW))

    helper(1024, middle)
