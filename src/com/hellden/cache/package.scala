package com.hellden

package object cache:

  class ObjectIdentityKey(val value: AnyRef):

    override def hashCode(): Int =
      System.identityHashCode(value)

    override def equals(obj: Any): Boolean = obj match
      case that: ObjectIdentityKey =>
        this.value eq that.value
      case _ =>
        false
