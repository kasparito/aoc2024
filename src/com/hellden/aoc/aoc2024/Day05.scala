package com.hellden.aoc.aoc2024

import com.hellden.collection.*
import com.hellden.grid.Direction.*
import com.hellden.grid.{Direction, Grid}

import scala.language.experimental.namedTuples

object Day05 extends Day(5):

  private val (pageOrdering, updates) =
    val lineIterator = inputLines.iterator

    val rules = lineIterator
      .takeWhile(_ != "")
      .map: line =>
        val Seq(p1, p2) = line.split('|').map(_.toLong).toSeq
        (p1, p2)
      .toSet
    val ordering = Ordering.fromLessThan[Long](rules (_, _))

    (ordering, lineIterator.map(_.split(',').map(_.toLong).toList).toSeq)

  private val sortedUpdates =
    updates.map: update =>
      (update, update.sorted(pageOrdering))

  override def part1: Long = // 5948
    sortedUpdates
      .collect:
        case (update, sortedUpdate) if update == sortedUpdate =>
          update.middle
      .sum

  override def part2: Long = // 3062
    sortedUpdates
      .collect:
        case (update, sortedUpdate) if update != sortedUpdate =>
          sortedUpdate.middle
      .sum
