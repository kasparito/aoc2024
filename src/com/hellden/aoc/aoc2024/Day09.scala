package com.hellden.aoc.aoc2024

import scala.annotation.tailrec

object Day09 extends Day(9):

  private enum Block(val index: Int, val size: Int):
    case Space(override val index: Int, override val size: Int) extends Block(index, size)
    case File(id: Int, override val index: Int, override val size: Int) extends Block(index, size)

  import Block.*

  private val blocks = inputLines
    .head
    .zipWithIndex
    .map:
      case (size, index) if index % 2 == 0 =>
        File(index / 2, index, size.toString.toInt)
      case (size, index) =>
        Space(index, size.toString.toInt)

  @tailrec
  private def checksum(blocks: Vector[Block], position: BigInt = 0, acc: BigInt = 0): BigInt =
    blocks match
      case File(id, _, size) +: tail =>
        checksum(tail, position + size, acc + id * (size * position + size * (size - 1) / 2))
      case head +: tail =>
        checksum(tail, position + head.size, acc)
      case _ =>
        acc

  private def mkString(blocks: Iterable[Block]): String = blocks
    .map:
      case Space(_, size) => Array.fill(size)(".").mkString
      case File(id, _, size) => Array.fill(size)(id).mkString
    .mkString

  override def part1: BigInt = // 6430446922192

    @tailrec
    def checksum(block: Block, blockPosition: BigInt, lastFile: File, acc: BigInt): BigInt =
      block match
        case _ if lastFile.index < block.index =>
          acc
        case File(id, index, _) if lastFile.id == id && lastFile.index == index =>
          acc + lastFile.id * (lastFile.size * blockPosition + lastFile.size * (lastFile.size - 1) / 2)
        case Space(index, size) if size > lastFile.size =>
          checksum(
            Space(index, size - lastFile.size),
            blockPosition + lastFile.size,
            blocks(lastFile.index - 2).asInstanceOf[File],
            acc + lastFile.id * (lastFile.size * blockPosition + lastFile.size * (lastFile.size - 1) / 2)
          )
        case Space(index, size) =>
          checksum(
            blocks(index + 1),
            blockPosition + size,
            lastFile.copy(size = lastFile.size - size),
            acc + lastFile.id * (size * blockPosition + size * (size - 1) / 2)
          )
        case File(id, index, size) =>
          checksum(
            blocks(index + 1),
            blockPosition + size,
            lastFile,
            acc + id * (size * blockPosition + size * (size - 1) / 2)
          )

    checksum(blocks.head, 0, blocks.last.asInstanceOf[File], 0)

  override def part2: BigInt = // 6460170593016
    checksum(
      blocks
        .reverseIterator
        .foldLeft(blocks):
          case (blocks, file: Block.File) =>
            val (prefix, suffix) = blocks.span:
              case Space(_, size) => size < file.size
              case File(id, _, _) => id != file.id
            suffix match
              case Space(index, size) +: tail =>
                prefix ++: file +: Space(index, size - file.size) +: tail.map:
                  case remove: File if remove.id == file.id => Space(remove.index, remove.size)
                  case block => block
              case _ =>
                blocks
          case (blocks, _) =>
            blocks
        .toVector
    )
