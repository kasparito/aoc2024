package com.hellden.aoc.aoc2024

import scala.collection.mutable
import scala.language.experimental.namedTuples
import scala.language.implicitConversions

object Day11 extends Day(11):

  private type Num = Long

  private def parse(s: String): List[Num] =
    s.split(" ").map(_.toLong).toList

  private object Split:
    def unapply(n: Num): Option[(Num, Num)] =
      val s = n.toString
      Option.when(s.length % 2 == 0):
        val (s1, s2) = s.splitAt(s.length / 2)
        (s1.toLong, s2.toLong)

  private val cache = mutable.Map.empty[(stone: Num, n: Int), Num]

  private def blink(stone: Num): List[Num] = stone match
    case stone if stone == 0 =>
      List(1)
    case Split(stone1, stone2) =>
      List(stone1, stone2)
    case stone =>
      List(2024 * stone)

  private def blink(stones: List[Num], n: Int = 1, acc: Num = 0): Num =
    stones match
      case _ if n == 0 =>
        stones.length
      case Nil =>
        acc
      case stone :: rest =>
        blink(rest, n, acc + cache.getOrElseUpdate((stone, n), blink(blink(stone), n - 1)))

  override def part1: Num = // 191690
    blink(parse(inputLines.head), n = 25)

  override def part2: Num = // 228651922369703
    blink(parse(inputLines.head), n = 75)
