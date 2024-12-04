package com.hellden.grid

object Grid:
  def apply(lines: Iterable[String]): Grid[Char] =
    new Grid(lines.toIndexedSeq.map(_.toIndexedSeq))

class Grid[T](grid: IndexedSeq[IndexedSeq[T]]):

  val horizontalBounds: Range = 0 until grid.map(_.length).max
  val verticalBounds: Range = grid.indices

  def positions: Iterable[Position] =
    for
      x <- horizontalBounds: Iterable[Int]
      y <- verticalBounds: Iterable[Int]
    yield Position(x, y)

  def isWithinBounds(x: Int, y: Int): Boolean =
    horizontalBounds.contains(x) && verticalBounds.contains(y)

  def move(position: Position, direction: Direction, steps: Int = 1): Option[(Position, T)] =
    val p = position.move(direction, steps)
    at(p).map((p, _))

  def at(position: Position): Option[T] =
    Option.when(isWithinBounds(position.x, position.y)):
      grid(position.y)(position.x)

  def at(position: Position, direction: Direction, steps: Int = 0): Option[T] =
    at(position.move(direction, steps))

  def find(v: T): Iterable[Position] =
    for
      position <- positions
      value <- at(position)
      if value == v
    yield
      position
