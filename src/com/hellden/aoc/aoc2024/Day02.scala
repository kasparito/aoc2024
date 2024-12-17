package com.hellden.aoc.aoc2024

object Day02 extends Day(2):

  override def part1: Long = // 213
    countSafeReports(tolerance = 0)

  override def part2: Long = // 285
    countSafeReports(tolerance = 1)

  private def countSafeReports(tolerance: Int): Int =
    inputLines
      .map: line =>
        line.split(" ").toIndexedSeq.map(_.toInt)
      .count: report =>
        val ok = math.min(errors(report), errors(report.reverse)) <= tolerance
        report.toList match
          case r@l1 :: l2 :: rest if ok && l1 == l2 =>
            println(r)
          case _ =>
        ok

  private val safeRange = 1 to 3

  private def errors(report: Seq[Int]): Int =
    report.sliding(2).count:
      case Seq(l1, l2) => !safeRange.contains(l2 - l1)
