package com.hellden.aoc.aoc2024

import scala.language.experimental.namedTuples

object Day03 extends Day(3):

  private val Do = """do\(\)""".r
  private val Dont = """don't\(\)""".r
  private val Multiplication = """mul\((\d{1,3}),(\d{1,3})\)""".r

  private val Instruction = s"$Do|$Dont|$Multiplication".r

  override def part1: Long = // 187825547
    Multiplication
      .findAllMatchIn(input)
      .map: m =>
        m.group(1).toLong * m.group(2).toLong
      .sum

  override def part2: Long = // 85508223
    Instruction
      .findAllIn(input)
      .foldLeft((sum = 0L, enabled = true)):
        case ((sum, _), Do()) =>
          (sum, true)
        case ((sum, _), Dont()) =>
          (sum, false)
        case ((sum, enabled), Multiplication(a, b)) if enabled =>
          (sum + a.toLong * b.toLong, enabled)
        case (result, _) =>
          result
      .sum
