package com.hellden.aoc.aoc2024

import com.hellden.grid.Direction.E
import com.hellden.grid.{BoundedGrid, Direction, Grid, Position}

import scala.annotation.tailrec
import scala.language.experimental.namedTuples
import scala.language.{implicitConversions, postfixOps}

object Day15 extends Day(15):

  type Num = BigInt

  private val (mapInput, moves) =
    val lineIterator = inputLines.iterator
    (
      lineIterator.takeWhile(_.headOption.contains('#')).toSeq,
      lineIterator
        .flatten
        .map:
          case '^' => Direction.N
          case 'v' => Direction.S
          case '<' => Direction.W
          case '>' => Direction.E
          case char => throw new IllegalArgumentException(s"Unknown direction: $char")
        .toSeq
    )

  private class Warehouse(width: Int):

    private val map: BoundedGrid[Char] = Grid(
      mapInput.map: line =>
        line.flatMap:
          case '.' => String(Array.fill(width)('.'))
          case '#' => String(Array.fill(width)('#'))
          case 'O' => String(Array('O') ++ Array.fill(width - 1)('.'))
          case '@' => String(Array('@') ++ Array.fill(width - 1)('.'))
          case char => throw new IllegalArgumentException(s"Unknown map object: $char")
    )

    import map.Cell

    private case class Box(positions: Set[Position]):

      def this(position: Position) =
        this((0 until width).map(offset => position.moveIn(E, offset)).toSet)

      def contains(position: Position): Boolean =
        positions.contains(position)

      def intersects(box: Box): Boolean =
        box.positions.exists(positions)

      def move(direction: Direction): Box =
        copy(positions = positions.map(_.moveIn(direction)))

      def coordinates: Num =
        BigInt(positions.head.y) * 100 + positions.head.x

    private case class State(robot: Cell, boxes: Set[Box])

    private val initialState: State = State(
      map.find('@').head,
      map.find('O').map(cell => new Box(cell.position)).toSet
    )

    private def move(state: State, direction: Direction): State =
      state
        .robot
        .move(direction)
        .filter(_.value != '#')
        .flatMap:
          case movedRobot@Cell(position, value) =>
            @tailrec
            def move(boxesToMove: List[Box], updatedBoxes: Set[Box]): Option[State] = boxesToMove match
              case Nil =>
                Some(State(movedRobot, updatedBoxes))
              case box :: tail =>
                val movedBox = box.move(direction)
                if movedBox.positions.flatMap(map.cellAt).exists(_.value == '#') then
                  None
                else
                  val obstructingBoxes = updatedBoxes.filter(_.intersects(movedBox))
                  move(obstructingBoxes.toList ::: tail, updatedBoxes -- obstructingBoxes + movedBox)
            val obstructingBoxes = state.boxes.filter(_.contains(position))
            move(obstructingBoxes.toList, state.boxes -- obstructingBoxes)
        .getOrElse(state)

    def run: Num =
      moves.foldLeft(initialState)(move).boxes.view.map(_.coordinates).sum

  override def part1: Num = // 1415498
    new Warehouse(1).run

  override def part2: Num = // 1432898
    new Warehouse(2).run
