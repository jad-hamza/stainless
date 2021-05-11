/* Copyright 2009-2021 EPFL, Lausanne */

package stainless.collection

import stainless.annotation._
import stainless.lang._

import CArray._

case class CArray[@mutable T](var underlying: List[T]) {
  require(intLength(underlying) >= 0)

  def apply(i: Int): T = {
    require(0 <= i)
    require(i < intLength(underlying))
    intApply(underlying, i)
  }

  def update(i: Int, v: T): Unit = {
    // underlying = underlying.updated(i, v)
  }

  def length: Int = {
    intLength(underlying)
  }.ensuring(res => res >= 0)
}

object CArray {
  // (conceptually) returns -1 if the list is too large for `Int`
  def intLength[@mutable T](l: List[T]): Int = l match {
    case Nil() => 0
    case Cons(x, xs) =>
      val tailLength = intLength(xs)
      if (tailLength == -1) -1
      else if (tailLength == 2147483647) -1 // reached maximum Int
      else tailLength + 1
  }

  def intApply[@mutable T](l: List[T], i: Int): T = {
    require(0 <= i && i < intLength(l))
    if (i == 0) l.head
    else intApply(l.tail, i-1)
  }

  def intUpdated[@mutable T](l: List[T], i: Int, v: T): List[T] = {
    require(0 <= i && i < intLength(l))
    if (i == 0) Cons(v, l.tail)
    else Cons(l.head, intUpdated(l.tail, i-1, v))
  }.ensuring(res =>
    intLength(res) == intLength(l) &&
    intApply(res, i) == v
  )

  @opaque @inlineOnce
  def applyUpdated[@mutable T](l: List[T], i: Int, v: T, j: Int): Unit = {
    require(0 <= i && i < intLength(l))
    require(0 <= j && j < intLength(l))
    require(i != j)
    decreases(j)

    if (j > 0 && i > 0) applyUpdated(l.tail, i-1, v, j-1)

  }.ensuring(_ =>
    intApply(intUpdated(l, i, v), j) == intApply(l, j)
  )
}
