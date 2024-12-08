package com.hellden.grid

import com.hellden.grid.Direction.*

import scala.collection.concurrent.TrieMap

object Grid:
  def apply(lines: Seq[String]): BoundedGrid[Char] =
    new BoundedGrid(0 until lines.map(_.length).max, lines.indices): (x, y) =>
      for
        line <- lines.lift(y)
        value <- line.lift(x)
      yield value

class Grid[T](values: (Int, Int) => Option[T]):

  case class Cell(position: Position, value: T):

    def move(direction: Direction, steps: Int = 1): Option[Cell] =
      cellAt(position.moveIn(direction, steps))

    def move(dx: Int, dy: Int): Option[Cell] =
      cellAt(position.move(dx, dy))

    def cellsIn(direction: Direction, offset: Int = 0): LazyList[Cell] =
      cellsFrom(position, direction, offset)

    def cellsInDirection(dx: Int, dy: Int): LazyList[Cell] =
      cellsFrom(position, dx: Int, dy: Int)

    def valuesIn(direction: Direction, offset: Int = 0): LazyList[T] =
      cellsIn(direction, offset).map(_.value)

  private val cellMap = TrieMap.empty[Position, Option[Cell]].withDefault: position =>
    values(position.x, position.y).map(Cell(position, _))

  def cellAt(position: Position): Option[Cell] =
    cellMap(position)

  def cellsFrom(position: Position, direction: Direction, offset: Int = 0): LazyList[Cell] =
    LazyList.from(offset).map(position.moveIn(direction, _)).map(cellAt).takeWhile(_.isDefined).flatten

  def cellsFrom(position: Position, dx: Int, dy: Int): LazyList[Cell] =
    LazyList.from(0).map(position.move(dx, dy, _)).map(cellAt).takeWhile(_.isDefined).flatten

class BoundedGrid[T](horisontalBounds: Range, verticalBounds: Range)(values: (Int, Int) => Option[T])
  extends Grid[T](values):

  def rows: IndexedSeq[Iterable[Cell]] = verticalBounds.map: y =>
    cellsFrom(Position(0, y), E)

  def columns: IndexedSeq[Iterable[Cell]] = horisontalBounds.map: x =>
    cellsFrom(Position(x, 0), S)

  def cells: Iterable[Cell] = rows.flatten

  def find(value: T): Iterable[Cell] =
    cells.filter(_.value == value)
