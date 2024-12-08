package com.hellden.grid

case class Position(x: Int, y: Int):

  def moveIn(d: Direction, steps: Int = 1): Position =
    move(d.dx, d.dy, steps)

  def move(dx: Int, dy: Int, steps: Int = 1): Position =
    Position(x + steps * dx, y + steps * dy)

object Position:
  given Ordering[Position] = Ordering.by(p => (p.x, p.y))
