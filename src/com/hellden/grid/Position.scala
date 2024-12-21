package com.hellden.grid

import scala.util.Try

case class Position(x: Int, y: Int):

  def moveIn(d: Direction, steps: Int = 1): Position =
    move(d.dx, d.dy, steps)

  def move(dx: Int, dy: Int, steps: Int = 1): Position =
    Position(x + steps * dx, y + steps * dy)
    
  def manhattanDistance(position: Position): Int =
    (position.x - x).abs + (position.y - y).abs

object Position:

  given Ordering[Position] = Ordering.by(p => (p.x, p.y))

  def parse(s: String): Position =
    Try(s.split(",")).toOption match
      case Some(Array(x, y)) => Position(x.toInt, y.toInt)
      case _ => throw IllegalArgumentException(s)
