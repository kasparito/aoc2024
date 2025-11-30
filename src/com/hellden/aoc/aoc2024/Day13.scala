package com.hellden.aoc.aoc2024

import com.hellden.number.*

import scala.annotation.{tailrec, targetName}
import scala.language.experimental.namedTuples
import scala.language.implicitConversions

object Day13 extends Day(13):

  val input2: String =
    """Button A: X+94, Y+34
      |Button B: X+22, Y+67
      |Prize: X=8400, Y=5400
      |
      |Button A: X+26, Y+66
      |Button B: X+67, Y+21
      |Prize: X=12748, Y=12176
      |
      |Button A: X+17, Y+86
      |Button B: X+84, Y+37
      |Prize: X=7870, Y=6450
      |
      |Button A: X+69, Y+23
      |Button B: X+27, Y+71
      |Prize: X=18641, Y=10279
      |""".stripMargin

  type Num = BigInt

  private case class Vector(x: Num, y: Num):
    def positive: Boolean = x >= 0 && y >= 0
    def tokens: Num = x * 3 + y
    @targetName("add") infix def +(v: Vector): Vector = Vector(x + v.x, y + v.y)
    @targetName("subtract") infix def -(v: Vector): Vector = Vector(x - v.x, y - v.y)
    @targetName("multiply") infix def *(n: Num): Vector = Vector(x * n, y * n)
    @targetName("divide") infix def /(n: Num): Vector = Vector(x / n, y / n)

  private object Vector:
    val zero: Vector = Vector(0, 0)
    given Ordering[Vector] = Ordering.by(_.tokens)

  private val ButtonPattern = """[^:]+: X.(\d+), Y.(\d+)""".r

  private def parseXY(s: String): Vector =
    s match
      case ButtonPattern(x, y) => Vector(x.toInt, y.toInt)
      case _ => throw IllegalArgumentException(s)

  private def gcd(v1: Vector, v2: Vector, v3: Vector): Num =
    v1.x.gcd(v1.y).gcd(v2.x).gcd(v2.y).gcd(v3.x).gcd(v3.y)

  private def tokens(a: Vector, b: Vector, p: Vector): Option[Num] =
    if a.x.even && b.x.even && p.x.odd || a.y.even && b.y.even && p.y.odd then
      None
    else if a.x.even == a.y.even && b.x.even == b.y.even && a.x.even != b.x.even && p.x.even != p.y.even then
      None
    else if a.x.odd && a.y.odd && b.x.odd && b.y.odd && p.x.even != p.y.even then
      None
    else
      val maxA = p.x / a.x min p.y / a.y
      // val maxB = p.x / b.x min p.y / b.y
      val solutions = for
        numA <- LazyList.iterate(maxA)(_ - 1).takeWhile(_ >= 0)
        remaining = p - a * numA
        numB = remaining.x / b.x
        if b * numB == remaining
      yield numA * 3 + numB
      solutions.headOption

  private val machines = inputLines
    .grouped(4)
    .map: machine =>
      val a = parseXY(machine(0))
      val b = parseXY(machine(1))
      val p = parseXY(machine(2))
      (a, b, p)

  override def part1: Num = // 28753
    machines.flatMap(tokens).sum

  override def part2: Num = // ???
    val correctedMachines = machines.map: (a, b, p) =>
      (a, b, p.copy(x = p.x + 10000000000000L, y = p.y + 10000000000000L))
    correctedMachines.flatMap(tokens).sum

  def factorize(x: Num): Map[Num, Int] =
    @tailrec
    def helper(x: Num, a: Int = 2, acc: List[Num] = Nil): List[Num] =
      if a * a > x then
        x :: acc
      else if x % a == 0 then
        helper(x / a, a, a :: acc)
      else
        helper(x, a + 1, acc)
    helper(x).groupBy(identity).view.mapValues(_.length).toMap
