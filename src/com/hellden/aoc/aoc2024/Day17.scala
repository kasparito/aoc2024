package com.hellden.aoc.aoc2024

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.experimental.namedTuples
import scala.language.implicitConversions
import scala.util.Try

object Day17 extends Day(17):

  type Num = Long

  private object Computer:

    private enum Opcode:
      case ADV
      case BXL
      case BST
      case JNZ
      case BXC
      case OUT
      case BDV
      case CDV

    private object Opcode:
      def unapply(op: Int): Option[Opcode] =
        Try(Opcode.fromOrdinal(op)).toOption

    import Opcode.*

    private enum Operand:
      case ZERO
      case ONE
      case TWO
      case THREE
      case A
      case B
      case C
      case RESERVED
      case Literal(value: Num)

    import Operand.*

    private object Combo:
      def unapply(op: Int): Option[Operand] =
        Try(Operand.fromOrdinal(op)).toOption

    case class State(a: Num, b: Num, c: Num)

    def run(
      program: IndexedSeq[Int],
      initialState: State,
      targetOutput: Option[IndexedSeq[Int]] = None
    ): IndexedSeq[Int] =

      @tailrec
      def run(position: Int, state: State, output: Vector[Int]): Vector[Int] =

        implicit def valueOf(operand: Operand): Num =
          operand match
            case ZERO => 0
            case ONE => 1
            case TWO => 2
            case THREE => 3
            case A => state.a
            case B => state.b
            case C => state.c
            case Literal(value) => value
            case _ => throw IllegalStateException()

        val nextPosition = position + 2
        program.slice(position, nextPosition).toList match
          case ops if ops.isEmpty || !targetOutput.forall(_.startsWith(output)) =>
            output
          case Opcode(ADV) :: Combo(operand) :: _ =>
            run(
              nextPosition,
              state.copy(a = A >> operand.toInt),
              output
            )
          case Opcode(BXL) :: literal :: _ =>
            run(
              nextPosition,
              state.copy(b = B ^ literal),
              output
            )
          case Opcode(BST) :: Combo(operand) :: _ =>
            run(
              nextPosition,
              state.copy(b = operand & 7),
              output
            )
          case Opcode(JNZ) :: literal :: _ =>
            // println(state)
            run(
              if valueOf(A) == 0 then nextPosition else literal,
              state,
              output
            )
          case Opcode(BXC) :: _ :: _ =>
            run(
              nextPosition,
              state.copy(b = B ^ C),
              output
            )
          case Opcode(OUT) :: Combo(operand) :: _ =>
            run(
              nextPosition,
              state,
              output :+ (operand & 7).toInt
            )
          case Opcode(BDV) :: Combo(operand) :: _ =>
            run(
              nextPosition,
              state.copy(b = A >> operand.toInt),
              output
            )
          case Opcode(CDV) :: Combo(operand) :: _ =>
            run(
              nextPosition,
              state.copy(c = A >> operand.toInt),
              output
            )
          case ops =>
            throw IllegalStateException(ops.mkString(","))

      run(0, initialState, Vector.empty)

  import Computer.State

  private val program = "2,4,1,2,7,5,4,7,1,3,5,5,0,3,3,0".split(',').toIndexedSeq.map(_.toInt)
  private val state = State(35200350, 0, 0)

  override def part1: String = // 2,7,4,7,2,1,7,5,1
    Computer.run(program, state).mkString(",")

  override def part2: Num = // 37221274271220
    var longest = 0
    var reg: Num = 37221274271220L

    while longest < program.length do

      var len = 0
      var a: Num = reg

      while a != 0 do
        val b = (a & 7) ^ 2
        if (((b ^ (a >> b)) ^ 3) & 7) == program(len) then
          len = len + 1
          a = a >> 3
        else
          a = 0

      if len > longest then
        longest = len
        println(s"$reg -> ${program.take(longest).mkString(",")}")

      reg = reg + 1

    reg - 1
