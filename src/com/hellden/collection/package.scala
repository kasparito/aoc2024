package com.hellden

package object collection:
  extension [A](i: Iterable[A])
    def cross[B](s: Seq[B]): Iterable[(A, B)] =
      for
        e1 <- i
        e2 <- s
      yield (e1, e2)
