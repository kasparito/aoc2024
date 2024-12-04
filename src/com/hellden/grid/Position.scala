package com.hellden.grid

case class Position(x: Int, y: Int):
  def move(d: Direction, steps: Int = 1): Position =
    Position(x + d.dx * steps, y + d.dy * steps)
