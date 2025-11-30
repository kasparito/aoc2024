package com.hellden.aoc.aoc2024

import com.hellden.grid.{Direction, Grid, Position}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.experimental.namedTuples
import scala.language.implicitConversions

object Day12 extends Day(12):

  type Num = Long

  override val input: String =
    """RRRRIICCFF
      |RRRRIICCCF
      |VVRRRCCFFF
      |VVRCCCJFFF
      |VVVVCJJCFE
      |VVIVCCJJEE
      |VVIIICJJEE
      |MIIIIIJJEE
      |MIIISIJEEE
      |MMMISSJEEE""".stripMargin

  private val garden = Grid(inputLines)
  import garden.Cell

  case class Region(flower: Char, positions: Set[Position]):

    def adjacent(position: Position): Boolean =
      Direction.NESW.map(position.moveIn(_)).exists(positions)

    def adjacent(region: Region): Boolean =
      region.positions.exists(adjacent)

    def merge(region: Region): Region =
      copy(positions = positions ++ region.positions)

    def area: Num =
      positions.size

    def perimeter: Num =
      positions
        .map: position =>
          Direction.NESW.map(position.moveIn(_)).count: adjacent =>
            !positions.contains(adjacent)
        .sum

  @tailrec
  private def merge(regions1: List[Region], regions2: List[Region]): List[Region] =
    (regions1, regions2) match
      case (Nil, regions) =>
        regions
      case (regions, Nil) =>
        regions
      case (region1 :: tail1, region2 :: tail2) if region1.adjacent(region2) =>
        merge(tail1, region1.merge(region2) :: tail2)
      case (region1 :: tail1, region2 :: tail2) =>
        merge(tail1, regions2)

  private def price(flower: Char, cells: Iterable[Cell]): Num =
    println(flower)
    val regions = cells.map(cell => Region(cell.value, Set(cell.position))).toList
    merge(regions, regions)
      .map: region =>
        println(region)
        region.area * region.perimeter
      .sum

  override def part1: Num = // ???
    garden.cells.groupBy(_.value).map(price).sum
