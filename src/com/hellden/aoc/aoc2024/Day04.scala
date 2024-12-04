package com.hellden.aoc.aoc2024

import scala.language.experimental.namedTuples

object Day04 extends Day(4):

  private case class Position(x: Int, y: Int):
    def move(d: Direction): Position =
      Position(x + d.dx, y + d.dy)

  private enum Direction(val dx: Int, val dy: Int):
    case N extends Direction(0, -1)
    case E extends Direction(1, 0)
    case W extends Direction(-1, 0)
    case S extends Direction(0, 1)
    case NE extends Direction(1, -1)
    case NW extends Direction(-1, -1)
    case SE extends Direction(1, 1)
    case SW extends Direction(-1, 1)

    def opposite: Direction = this match
      case N => S
      case E => W
      case W => E
      case S => N
      case NE => SW
      case NW => SE
      case SE => NW
      case SW => NE

  import Direction._

  private val xRange = inputLines.indices
  private val yRange = inputLines.head.indices

  private def charAt(p: Position): Option[Char] =
    Option.when(xRange.contains(p.x) && yRange.contains(p.y)):
      inputLines(p.x)(p.y)

  private def positions =
    for
      x <- xRange.iterator
      y <- yRange.iterator
    yield Position(x, y)

  private def countXMASFrom(p: Position): Int =
    Direction.values.count: direction =>
      charAt(p.move(direction)).contains('M') &&
      charAt(p.move(direction).move(direction)).contains('A') &&
      charAt(p.move(direction).move(direction).move(direction)).contains('S')

  private def containsXMAS(p: Position): Boolean =
    def check(d: Direction): Boolean =
      charAt(p.move(d)).contains('M') && charAt(p.move(d.opposite)).contains('S')
    charAt(p).contains('A') && (check(NW) || check(SE)) && (check(NE) || check(SW))

  override def part1: Long = // 2464
    positions
      .collect:
        case p if charAt(p).contains('X') =>
          countXMASFrom(p)
      .sum

  override def part2: Long = // 1982
    positions.count(containsXMAS)
