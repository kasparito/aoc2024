package com.hellden

package object collection:

  extension [A](i: Iterable[A])
    def cross[B](s: Seq[B]): Iterable[(A, B)] =
      for
        e1 <- i
        e2 <- s
      yield (e1, e2)

  extension [T](s: Seq[T])
    def middle: T =
      if s.length % 2 == 0 then
        throw new NoSuchElementException(s"middle of even sized (${s.length}) sequence")
      else
        s(s.length / 2)
