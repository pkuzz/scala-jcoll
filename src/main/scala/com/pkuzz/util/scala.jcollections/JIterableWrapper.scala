package com.pkuzz.util.scala.jcollections

import java.lang.{Iterable => JIterable}
import java.util.{ArrayList => JArrayList, Collection => JCollection, HashMap => JHashMap, HashSet => JHashSet, List => JList, Set => JSet, TreeSet => JTreeSet}


class JIterableWrapper[A](underlying: JIterable[A]) {
  def foreach(fun: A => Unit): Unit = {
    val it = underlying.iterator
    while(it.hasNext) fun(it.next())
  }

  def find(fun: A => Boolean): Option[A] = {
    val it = underlying.iterator
    while(it.hasNext) {
      val el = it.next()
      if(fun(el)) return Some(el)
    }
    None
  }

  def count(fun: A => Boolean): Int = {
    var cnt = 0
    val it = underlying.iterator
    while(it.hasNext) {
      val el = it.next()
      if(fun(el)) cnt += 1
    }
    cnt
  }

  def exists(fun: A => Boolean): Boolean = {
    val it = underlying.iterator
    var res = false
    while (!res && it.hasNext) res = fun(it.next())
    res
  }

  def forall(fun: A => Boolean): Boolean = {
    val it = underlying.iterator
    var res = true
    while (res && it.hasNext) res = fun(it.next())
    res
  }

  def first: A = underlying.iterator.next()
  def head: A = first

  def firstOpt: Option[A] = {
    val it = underlying.iterator()
    if (it.hasNext) Some(it.next()) else None
  }

  def headOption: Option[A] = firstOpt

  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    val it = underlying.iterator
    while(it.hasNext) {
      val el = it.next()
      result = op(result, el)
    }
    result
  }

  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)
  def sumBy[B](mapper: A => B)(implicit num: Numeric[B]): B = foldLeft(num.zero)((a, it) => num.plus(a, mapper(it)))

  def min(implicit cmp: Ordering[A]): A = {
    val it = underlying.iterator()
    if (!it.hasNext) throw new UnsupportedOperationException("empty.min")
    var min = it.next()
    while (it.hasNext) {
      val el = it.next()
      if (cmp.lteq(el, min)) min = el
    }
    min
  }

  def isNotEmpty: Boolean = underlying.iterator().hasNext
  def nonEmpty: Boolean = underlying.iterator().hasNext
  def isEmpty: Boolean = !underlying.iterator().hasNext

  def filter(fun: A => Boolean): JIterable[A] = filter[JCollection[A]](fun, newCollection)
  def partition(fun: A => Boolean): (JIterable[A], JIterable[A]) = partition[JCollection[A]](fun, newCollection, newCollection)

  def map[B](fun: A => B): JIterable[B] = map[B, JCollection[B]](fun, newCollection)

  def flatMap[B](fun: A => JCollection[B]): JIterable[B] = flatMap[B, JCollection[B]](fun, newCollection)

  def toMap[F, S](implicit ev: A <:< (F, S)): JHashMap[F, S] = {
    val res = new JHashMap[F, S]
    val it = underlying.iterator
    while(it.hasNext) {
      val el = it.next().asInstanceOf[(F, S)]
      res.put(el._1, el._2)
    }
    res
  }

  protected def filter[B <: JCollection[A]](function: A => Boolean, col: B): B = {
    val it = underlying.iterator
    while(it.hasNext) {
      val el = it.next()
      if(function(el)) col.add(el)
    }
    col
  }

  protected def partition[B <: JCollection[A]](function: A => Boolean, col1: B, col2: B): (B, B) = {
    val it = underlying.iterator
    while(it.hasNext) {
      val el = it.next()
      if(function(el)) col1.add(el) else col2.add(el)
    }
    (col1, col2)
  }

  protected def map[B, C <: JCollection[B]](function: A => B, col: C): C = {
    val it = underlying.iterator
    while(it.hasNext) col.add(function(it.next()))
    col
  }

  protected def flatMap[B, C <: JCollection[B]](function: A => JCollection[B], col: C): C = {
    val it = underlying.iterator
    while(it.hasNext) col.addAll(function(it.next()))
    col
  }

  private def newCollection[B]: JCollection[B]  = {
    underlying match {
      case ts: JTreeSet[_] => new JTreeSet[B]()
      case s: JSet[_] => new JHashSet[B]()
      case _ => new JArrayList[B]()
    }
  }
}