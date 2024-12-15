package com.hellden.aoc.aoc2024

import com.hellden.grid.{Direction, Position}

import scala.language.experimental.namedTuples
import scala.language.implicitConversions

object Day14 extends Day(14):

  private type Num = Int

  private val width = 101
  private val height = 103

  private case class Robot(position: Position, dx: Int, dy: Int):
    def move(steps: BigInt): Robot =
      copy(position = Position(
        ((BigInt(position.x) + steps * dx) % width).toInt,
        ((BigInt(position.y) + steps * dy) % height).toInt
      ))

  private object Robot:

    private val Pattern = """p=([-\d]+),([-\d]+) v=([-\d]+),([-\d]+)""".r

    def parse(s: String): Option[Robot] =
      s match
        case Pattern(px, py, vx, vy) =>
          val dx = vx.toInt
          val dy = vy.toInt
          Some(Robot(
            Position(px.toInt, py.toInt),
            if dx >= 0 then dx else width + dx % width,
            if dy >= 0 then dy else height + dy % height
          ))
        case _ =>
          None

  private val robots = inputLines.flatMap(Robot.parse)

  override def part1: Int = // 209409792
    val vMid = width / 2
    val hMid = height / 2
    robots
      .map(_.move(100).position)
      .collect:
        case Position(x, y) if x < vMid && y < hMid => Direction.NW
        case Position(x, y) if x < vMid && y > hMid => Direction.SW
        case Position(x, y) if x > vMid && y < hMid => Direction.NE
        case Position(x, y) if x > vMid && y > hMid => Direction.SE
      .groupBy(identity)
      .values
      .map(_.size)
      .product

  override def part2: Num = // 8006
    LazyList
      .from(0)
      .find: seconds =>
        val robotsByPosition = robots.map(_.move(seconds)).groupBy(_.position)
        val lines = (0 until height).map: y =>
          String((0 until width)
            .map:
              case x if robotsByPosition.isDefinedAt(Position(x, y)) =>
                'A'
              case _ =>
                ' '
            .toArray
          )
        val isTree = lines.exists(_.contains("AAAAAAAAAAAAAAAAAAAAA"))
        if isTree then lines.foreach(println)
        isTree
      .head
