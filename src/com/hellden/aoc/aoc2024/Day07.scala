package com.hellden.aoc.aoc2024

import scala.annotation.targetName

object Day07 extends Day(7):

  private case class Equation(testValue: BigInt, numbers: List[BigInt]):
    def hasSolution(ops: (BigInt, BigInt) => BigInt*): Boolean =
      def helper(numbers: List[BigInt], acc: BigInt): Boolean =
        numbers match
          case Nil =>
            acc == testValue
          case head :: tail if acc <= testValue =>
            ops.exists(op => helper(tail, op(acc, head)))
          case _ =>
            false
      helper(numbers.tail, numbers.head)

  private val equations =
    inputLines.map: line =>
      val numbers = line.replace(": ", " ").split(' ').map(BigInt.apply).toList
      Equation(testValue = numbers.head, numbers = numbers.tail)

  extension (i: BigInt)
    @targetName("concat")
    def ||(j: BigInt): BigInt =
      BigInt(s"$i$j")

  private def totalCalibrationResult(ops: (BigInt, BigInt) => BigInt*): BigInt =
    equations.filter(_.hasSolution(ops*)).map(_.testValue).sum

  override def part1: BigInt = // 1430271835320
    totalCalibrationResult(_ * _, _ + _)

  override def part2: BigInt = // 456565678667482
    totalCalibrationResult(_ || _, _ * _, _ + _)
