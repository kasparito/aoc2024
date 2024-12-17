package com.hellden.aoc.aoc2024

import com.hellden.grid.*
import com.hellden.grid.Direction.*
import com.hellden.grid.Direction.Turn.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.experimental.namedTuples

object Day16 extends Day(16):

  type Num = Long

  private val maze = Grid(inputLines)
  import maze.Cell

  private val start = maze.find('S').head
  private val finish = maze.find('E').head

  private case class Path(cell: Cell, trail: List[(Position, Direction)], direction: Direction, score: Num):
    val key: (Position, Direction) = (cell.position, direction)
    private val estimatedScore: Num = score + cell.position.manhattanDistance(finish.position)

  private object Path:
    given Ordering[Path] = Ordering.by(path => -path.estimatedScore)

  private class PriorityQueue:

    private val startPath: Path = Path(start, List((start.position, E)), E, 0)
    private val queue = mutable.PriorityQueue(startPath)
    private val bestPaths = mutable.Map(startPath.key -> startPath)

    private def isEmpty: Boolean = queue.isEmpty

    private def enqueue(cell: Cell, trail: List[(Position, Direction)], direction: Direction, score: Num, maxScore: Num): Unit =
      cell.move(direction).foreach: nextCell =>
        if nextCell.value != '#' && !trail.contains(nextCell.position) && score <= maxScore then
          enqueue(Path(nextCell, (nextCell.position, direction) :: trail, direction, score))

    private def enqueue(path: Path): Unit =
      if bestPaths.get(path.key).forall(_.score >= path.score) then
        queue.enqueue(path)
        bestPaths.put(path.key, path)

    private def dequeue(): Path =
      queue.dequeue()

    @tailrec
    final def find(solutions: Set[Path] = Set.empty): Set[Path] =
      if isEmpty then
        solutions
      else
        dequeue() match
          case path@Path(Cell(_, 'E'), _, _, score) =>
            find(
              solutions.headOption match
                case Some(solution) if solution.score == score =>
                  solutions ++ Set(path)
                case Some(solution) if solution.score < score =>
                  solutions
                case _ =>
                  Set(path)
            )
          case path@Path(cell, trail, direction, score) =>
            val maxScore = solutions.headOption.map(_.score).getOrElse(Long.MaxValue)
            enqueue(cell, trail, direction, score + 1, maxScore)
            enqueue(cell, trail, direction.turn(Left), score + 1001, maxScore)
            enqueue(cell, trail, direction.turn(Right), score + 1001, maxScore)
            find(solutions)

  private val solutions = new PriorityQueue().find()

  override def part1: Num = // 105508
    solutions.head.score

  override def part2: Num = // 548
    solutions.flatMap(_.trail.map(_._1)).size
