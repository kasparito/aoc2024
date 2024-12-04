package com.hellden.grid

object Grid:
  def apply(lines: Iterable[String]): Grid[Char] =
    new Grid(lines.toIndexedSeq.map(_.toIndexedSeq))

class Grid[T](grid: IndexedSeq[IndexedSeq[T]]):

  val horizontalBounds: Range = 0 until grid.map(_.length).max
  val verticalBounds: Range = grid.indices

  def positions =
    for
      x <- horizontalBounds: Iterable[Int]
      y <- verticalBounds: Iterable[Int]
    yield Position(x, y)

  def isWithinBounds(x: Int, y: Int): Boolean =
    horizontalBounds.contains(x) && verticalBounds.contains(y)

  def move(position: Position, direction: Direction, steps: Int = 1): Option[(Position, T)] =
    val p = position.move(direction, steps)
    valueFor(p).map((p, _))

  def valueFor(position: Position): Option[T] =
    Option.when(isWithinBounds(position.x, position.y)):
      grid(position.y)(position.x)

  def find(v: T): Iterable[Position] =
    for
      position <- positions
      value <- valueFor(position)
      if value == v
    yield
      position
