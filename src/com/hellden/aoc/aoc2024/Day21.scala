package com.hellden.aoc.aoc2024

import com.hellden.collection.Channel.Consumer
import com.hellden.grid.*
import com.hellden.grid.Direction.*
import com.hellden.grid.PathFinder.*

import scala.annotation.tailrec

object Day21 extends Day(21):

  override val input: String = 
    """029A
      |980A
      |179A
      |456A
      |379A
      |""".stripMargin

  trait Keypad[T]:

    def gap(key: T): Boolean

    protected val grid: BoundedGrid[T]

    lazy val positions: Map[T, Position] =
      grid
        .cells
        .map: cell =>
          cell.value -> cell.position
        .toMap

    final def find(start: T, finish: T): Consumer[Path] =
      val pathFinder = new PathFinder(grid):
        override def allowed(value: T): Boolean = !gap(value)
        override def cost(direction: Direction, turn: Turn): PathFinder.Num =
          DirKey.positions(DirKey(direction)).manhattanDistance(DirKey.positions(DirKey(direction.turn(turn))))
      pathFinder.find(positions(start), positions(finish))

  enum NumKey:
    case Gap
    case A
    case Num(n: Int)

  object NumKey extends Keypad[NumKey]:

    override def gap(key: NumKey): Boolean = key == Gap

    override protected val grid: BoundedGrid[NumKey] = Grid.from(Seq(
      Seq(Num(7), Num(8), Num(9)),
      Seq(Num(4), Num(5), Num(6)),
      Seq(Num(1), Num(2), Num(3)),
      Seq(Gap, Num(0), A)
    ))

    def apply(char: Char): NumKey =
      char match
        case 'A' => A
        case n if n.isDigit => Num(String(Array(n)).toInt)

  given Keypad[NumKey] = NumKey

  enum DirKey:
    case Gap
    case Activate
    case Dir(direction: Direction)

  object DirKey extends Keypad[DirKey]:

    override def gap(key: DirKey): Boolean = key == Gap

    override protected val grid: BoundedGrid[DirKey] = Grid.from(Seq(
      Seq(Gap, Dir(N), Activate),
      Seq(Dir(W), Dir(S), Dir(E))
    ))

    def apply(direction: Direction): DirKey = 
      DirKey.Dir(direction)

  given Keypad[DirKey] = DirKey
  
  def toString(moves: Seq[DirKey]): String =
    val chars = moves.map:
      case DirKey.Activate => 'A'
      case DirKey.Dir(N) => '^'
      case DirKey.Dir(S) => 'v'
      case DirKey.Dir(W) => '<'
      case DirKey.Dir(E) => '>'
    String(chars.toArray)

  def sequences[T](from: T, to: T)(using keypad: Keypad[T]): Iterable[List[DirKey]] =
    keypad.find(from, to).best.map: solution =>
      val dirKeys = DirKey.Activate :: solution.trail.map: (_, direction) =>
        DirKey.Dir(direction)
      dirKeys.reverse.tail

  def shortestSequence[T](start: T, keys: List[T], trail: List[DirKey] = Nil)(using keypad: Keypad[T]): Iterable[List[DirKey]] =
    keys match
      case Nil =>
        Iterable(trail)
      case key :: tail =>
        sequences(start, key).flatMap(s => shortestSequence(key, tail, trail ::: s))
      
  override def part1: Long =
    inputLines
      .map: code =>
        val numPart = code.filterNot(_ == 'A').toLong
        val moves1 = shortestSequence(NumKey.A, code.map(NumKey.apply).toList).toSeq.sortBy(_.size)
        println(toString(moves1.head))
        val moves2 = moves1.flatMap(shortestSequence(DirKey.Activate, _))
        println(moves2.size)
        moves2.take(4).foreach(m => println(toString(m)))
        val moves3 = moves2.take(4).flatMap(shortestSequence(DirKey.Activate, _))
        println(moves3.size)
        moves3.take(4).foreach(m => println(toString(m)))
        moves3.minBy(_.size).length * numPart
      .sum
