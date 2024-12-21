package com.hellden.grid

import com.hellden.cache.ObjectIdentityKey
import com.hellden.collection.Channel
import com.hellden.collection.Channel.{Consumer, Producer}
import com.hellden.grid.Direction.*
import com.hellden.grid.Direction.Turn.*

import java.util.concurrent.LinkedTransferQueue
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.{SortedSet, mutable}
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object PathFinder:

  type Num = Long

  case class Solution(score: Num, trail: List[Position])
  case class Path(position: Position, direction: Direction, trail: List[(Position, Direction)], score: Num):
    def toSolution: Solution = Solution(score, trail.map(_._1))

class PathFinder[T](grid: BoundedGrid[T]):
  import PathFinder.*

  import grid.Cell

  def cost(turn: Turn): Num = 0

  def consider(score: Num, bestScore: Num): Boolean = score <= bestScore

  def done(position: Position): Boolean = false

  def heuristicScore(path: Path): Num = -path.score

  def key(path: Path): Any = path.position

  def find(direction: Direction, start: Position, finish: Position): Consumer[Solution] =
    PriorityQueue(direction, start, finish).find()

  def print(solution: Solution, value: T): Unit =
    println("\nSolution:\n=========")
    grid.verticalBounds.foreach: y =>
      val line = grid.horisontalBounds.map: x =>
        val cellPosition = Position(x, y)
        solution
          .trail
          .collectFirst:
            case position if position == cellPosition =>
              value
          .orElse(grid.cellAt(cellPosition).map(_.value))
          .getOrElse('.')
      println(line.mkString)

  private class PriorityQueue(direction: Direction, start: Position, finish: Position):

    private val startPath: Path = Path(start, direction, List((start, direction)), 0)
    private val queue = mutable.PriorityQueue(startPath):
      val cache = TrieMap.empty[ObjectIdentityKey, Num]
      Ordering.by(path => cache.getOrElseUpdate(ObjectIdentityKey(path), heuristicScore(path)))
    private val bestPaths = mutable.Map(key(startPath) -> startPath)

    private def enqueue(
      position: Position,
      direction: Direction,
      trail: List[(Position, Direction)],
      score: Num,
      maxScore: Num
    ): Unit =
      grid.cellAt(position.moveIn(direction)).foreach: nextCell =>
        if nextCell.value != '#' && !trail.contains(nextCell.position) && score <= maxScore then
          enqueue(Path(nextCell.position, direction, (nextCell.position, direction) :: trail, score))

    private def enqueue(path: Path): Unit =
      if bestPaths.get(key(path)).forall(_.score >= path.score) then
        queue.enqueue(path)
        bestPaths.put(key(path), path)

    private def dequeue(): Path =
      queue.dequeue()

    def find(): Consumer[Solution] =
      val finishScores = NESW.flatMap(d => bestPaths.get((finish, d))).map(_.score)
      val bestScore = if finishScores.isEmpty then Long.MaxValue else finishScores.min
      val solutions = Channel[Solution]
      Future(find(bestScore, solutions.producer))
      solutions.consumer

    @tailrec
    private def find(bestScore: Num, solutions: Producer[Solution]): Unit =
      if queue.isEmpty then
        solutions.done()
      else if solutions.active then
        dequeue() match
          case path if path.position == finish =>
            if consider(path.score, bestScore) then
              solutions.publish(path.toSolution)
            find(bestScore.min(path.score), solutions)
          case path if done(path.position) =>
            solutions.publish(path.toSolution)
            find(bestScore, solutions)
          case path@Path(position, direction, trail, score) =>
            enqueue(position, direction, trail, score + 1, bestScore)
            enqueue(position, direction.turn(Left), trail, score + 1 + cost(Left), bestScore)
            enqueue(position, direction.turn(Right), trail, score + 1 + cost(Right), bestScore)
            find(bestScore, solutions)
