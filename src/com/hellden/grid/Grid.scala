package com.hellden.grid

object Grid:
  def apply(lines: Iterable[String]): Grid[Char] =
    new Grid(lines.toIndexedSeq.map(_.toIndexedSeq))

class Grid[T](grid: IndexedSeq[IndexedSeq[T]]):

  case class Cell(position: Position, value: T):

    def move(direction: Direction, steps: Int = 1): Option[Cell] =
      cellAt(position.move(direction, steps))

    def cellsIn(direction: Direction, offset: Int = 0): LazyList[Cell] =
      cellsFrom(position, direction, offset)

    def valuesIn(direction: Direction, offset: Int = 0): LazyList[T] =
      cellsIn(direction, offset).map(_.value)

  val rows: IndexedSeq[IndexedSeq[Cell]] =
    grid.zipWithIndex.map: (row, y) =>
      row.zipWithIndex.map: (value, x) =>
        Cell(Position(x, y), value)

  def cells: Iterable[Cell] = rows.view.flatten

  def cellAt(position: Position): Option[Cell] =
    for
      row <- rows.lift(position.y)
      value <- row.lift(position.x)
    yield value

  def cellsFrom(position: Position, direction: Direction, offset: Int = 0): LazyList[Cell] =
    LazyList.from(offset).map(position.move(direction, _)).map(cellAt).takeWhile(_.isDefined).flatten

  def find(value: T): Iterable[Cell] =
    cells.filter(_.value == value)
