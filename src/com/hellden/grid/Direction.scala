package com.hellden.grid

import com.hellden.grid.Direction.Turn

object Direction:

  enum Diagonal(direction: Direction):
    case Criss extends Diagonal(NE)
    case Cross extends Diagonal(SE)
    
    val directions: Set[Direction] = Set(direction, direction.opposite)

  enum Turn(val sign: Int):
    case Left extends Turn(-1)
    case Right extends Turn(1)

enum Direction(val dx: Int, val dy: Int):
  case N extends Direction(0, -1)
  case NE extends Direction(1, -1)
  case E extends Direction(1, 0)
  case SE extends Direction(1, 1)
  case S extends Direction(0, 1)
  case SW extends Direction(-1, 1)
  case W extends Direction(-1, 0)
  case NW extends Direction(-1, -1)

  def opposite: Direction = this match
    case N => S
    case E => W
    case W => E
    case S => N
    case NE => SW
    case NW => SE
    case SE => NW
    case SW => NE

  def turn(to: Turn, steps: Int = 2): Direction =
    Direction.fromOrdinal((ordinal + to.sign * steps) % Direction.values.length)

  def move(position: Position, steps: Int = 1): Position =
    position.move(this, steps)
