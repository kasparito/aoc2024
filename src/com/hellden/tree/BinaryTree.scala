package com.hellden.tree

import com.hellden.tree.BinaryTree.{Empty, Matchable, Node}

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.infixOrderingOps

enum BinaryTree[+E : Ordering](val size: Int, val depth: Int):
  private case Empty extends BinaryTree[Nothing](0, 0)
  private case Node[+T : Ordering](
    value: T,
    left: BinaryTree[T],
    right: BinaryTree[T]
  ) extends BinaryTree[T](1 + left.depth + right.depth, 1 + math.max(left.depth, right.depth))

  private def removeMin: (BinaryTree[E], Option[E]) =
    this match
      case Empty => (Empty, None)
      case Node(value, Empty, right) => (right, Some(value))
      case node: Node[E] =>
        val (newLeft, value) = node.left.removeMin
        (Node(node.value, newLeft, node.right), value)

  private def removeMax: (BinaryTree[E], Option[E]) =
    this match
      case Empty => (Empty, None)
      case Node(value, left, Empty) => (left, Some(value))
      case node: Node[E] =>
        val (newRight, value) = node.right.removeMax
        (Node(node.value, node.left, newRight), value)

  private def insert[B >: E : Ordering](value: B): BinaryTree[B] =
    this match
      case Empty =>
        Node(value, Empty, Empty)
      case Node(nodeValue, left, right) if value < nodeValue =>
        val (newValue, newLeft, newRight) =
          if left.depth - right.depth <= 1 then
            (nodeValue, left.insert(value), right)
          else
            val (newLeft, newNodeValue) = left.insert(value).removeMax
            (newNodeValue.get, newLeft, right.insert(nodeValue))
        Node(newValue, newLeft, newRight)
      case Node(nodeValue, left, right) if value > nodeValue =>
        val (newValue, newLeft, newRight) =
          if right.depth - left.depth <= 1 then
            (nodeValue, left, right.insert(value))
          else
            val (newRight, newNodeValue) = right.insert(value).removeMin
            (newNodeValue.get, left.insert(nodeValue), newRight)
        Node(newValue, newLeft, newRight)
      case node: Node[_] =>
        node

  def find[K : Ordering, R](key: K)(using matchable: Matchable[E, K]): Option[E] =
    @tailrec
    def rec(tree: BinaryTree[E]): Option[E] =
      tree match
        case Empty => None
        case Node(value, left, right) =>
          if value.matching(key) then
            Some(value)
          else if key < value.key then
            rec(left)
          else
            rec(right)
    rec(this)

object BinaryTree:

  trait Matchable[-T, K]:
    extension (t: T)
      def key: K
      def matching(k: K): Boolean

  def apply[T : Ordering](values: Iterable[T]): BinaryTree[T] =
    values.foldLeft[BinaryTree[T]](Empty): (tree, value) =>
      tree.insert(value)
