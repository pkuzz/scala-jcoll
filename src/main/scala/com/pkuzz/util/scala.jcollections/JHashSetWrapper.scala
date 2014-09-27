package com.pkuzz.util.scala.jcollections

import java.util.{Set => JSet,  HashSet => JHashSet, Collection => JCollection}
import collection.mutable.ArrayBuffer


class JHashSetWrapper[A](underlying: JSet[A]) extends JIterableWrapper[A](underlying) {

  override def map[B](fun: A => B): JSet[B] = map(fun, new JHashSet[B]())
  override def flatMap[B](fun: A => JCollection[B]): JSet[B] = flatMap(fun, new JHashSet[B]())

  override def filter(fun: A => Boolean): JHashSet[A] = filter(fun, new JHashSet[A]())
  override def partition(fun: A => Boolean): (JHashSet[A], JHashSet[A]) = partition(fun, new JHashSet[A](), new JHashSet[A]())

  def toScalaBuffer: ArrayBuffer[A] = {
    val lst = new ArrayBuffer[A]
    val it = underlying.iterator
    while (it.hasNext) lst.append(it.next)
    lst
  }
}