package com.hellden.grid

object Grid:
  def apply(lines: Iterable[String]): Grid[Char] =
    new Grid(lines.toIndexedSeq.map(_.toIndexedSeq))

class Grid[T](grid: IndexedSeq[IndexedSeq[T]]):

  case class Cell(position: Position, value: T):

    def move(direction: Direction, steps: Int = 1): Option[Cell] =
      at(position.move(direction, steps))

    def valuesIn(direction: Direction, offset: Int = 0): LazyList[T] =
      valuesFrom(position, direction, offset)

  val rows: IndexedSeq[IndexedSeq[Cell]] =
    grid.zipWithIndex.map: (row, y) =>
      row.zipWithIndex.map: (value, x) =>
        Cell(Position(x, y), value)

  def cells: Iterable[Cell] = rows.view.flatten

  def at(position: Position): Option[Cell] =
    for
      row <- rows.lift(position.y)
      value <- row.lift(position.x)
    yield value

  def valuesFrom(position: Position, direction: Direction, offset: Int = 0): LazyList[T] =
    LazyList
      .from(offset)
      .map: steps =>
        position.move(direction, steps)
      .map(at)
      .takeWhile(_.isDefined)
      .collect:
        case Some(cell) =>
          cell.value

  def find(value: T): Iterable[Cell] =
    cells.filter(_.value == value)
