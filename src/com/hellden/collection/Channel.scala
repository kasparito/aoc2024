package com.hellden.collection

import java.util.concurrent.LinkedTransferQueue
import java.util.concurrent.atomic.AtomicBoolean

class Channel[T] extends Channel.Producer[T] with Channel.Consumer[T]:

  private enum Item:
    case Done
    case Value(v: T)

  private val queue = LinkedTransferQueue[Item]
  private val cancelled = AtomicBoolean()

  def producer: Channel.Producer[T] = this
  def consumer: Channel.Consumer[T] = this

  def active: Boolean =
    !cancelled.get()

  def publish(value: T): Unit =
    queue.put(Item.Value(value))

  def done(): Unit = cancel()

  def cancel(): Unit =
    cancelled.set(true)
    queue.put(Item.Done)

  def iterable: Iterable[T] =
    LazyList.continually(queue.take()).takeWhile(_ != Item.Done).collect:
      case Item.Value(value) => value

object Channel:

  trait Producer[T]:
    def active: Boolean
    def publish(value: T): Unit
    def done(): Unit

  trait Consumer[T]:
    def cancel(): Unit
    def iterable: Iterable[T]
    final def head: T = take(1).head
    final def take(n: Int): Iterable[T] =
      val values = iterable.take(n).toVector
      cancel()
      values
