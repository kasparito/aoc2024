package com.hellden.aoc.aoc2024

object Day01 extends Day(1):

  private val Pattern = """(\d+)\s+(\d+)""".r

  private val numbers = inputLines.map:
    case Pattern(left, right) => (left.toLong, right.toLong)
    case _ => throw new IllegalArgumentException

  private val left = numbers.map(_._1)
  private val right = numbers.map(_._2)

  override def part1: Long = // 2367773
    left.sorted.zip(right.sorted).map(_ - _).map(math.abs).sum

  override def part2: Long = // 21271939
    left.map(x => x * right.count(_ == x)).sum
