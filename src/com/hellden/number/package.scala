package com.hellden

package object number:

  extension (n: Int)
    inline def even: Boolean = n % 2 == 0
    inline def odd: Boolean = n % 2 == 1

  extension (n: Long)
    inline def even: Boolean = n % 2 == 0
    inline def odd: Boolean = n % 2 == 1

  extension (n: BigInt)
    inline def even: Boolean = n % 2 == 0
    inline def odd: Boolean = n % 2 == 1
