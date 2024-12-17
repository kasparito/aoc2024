package com.hellden.aoc.aoc2024

import scala.collection.Iterable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.io.Source

trait Day(day: Int):

  extension [T] (f: Future[T])
    def await: T = Await.result(f, Duration.Inf)

  extension [T] (x: T)
    def log(s: String): T =
      println(s"$s: $x")
      x

  val input: String =
    val source = Source.fromFile(f"input/day$day%02d.txt")
    try source.mkString finally source.close()

  lazy val inputLines: IndexedSeq[String] = 
    input.split('\n').toIndexedSeq

  def part1: Any = "TBD"

  def part2: Any = "TBD"

  def main(args: Array[String]): Unit =
    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
