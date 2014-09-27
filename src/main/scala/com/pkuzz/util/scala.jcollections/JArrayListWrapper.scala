package com.pkuzz.util.scala.jcollections

import java.util
import java.util.{Collections, Comparator, ArrayList => JArrayList, Collection => JCollection, HashMap => JHashMap, HashSet => JHashSet, List => JList, Map => JMap, Set => JSet}

class JArrayListWrapper[A](underlying: JList[A]) extends JIterableWrapper[A](underlying) {

  override def map[B](fun: A => B): JArrayList[B] = map(fun, new JArrayList[B]())
  override def flatMap[B](fun: A => JCollection[B]): JArrayList[B] = flatMap(fun, new JArrayList[B]())

  override def filter(fun: A => Boolean): JArrayList[A] = filter(fun, new JArrayList[A]())
  override def partition(fun: A => Boolean): (JArrayList[A], JArrayList[A]) = partition(fun, new JArrayList[A](), new JArrayList[A]())

  def removeDuplicatesInPlace(): JList[A] = {
    val set = new util.LinkedHashSet(underlying)
    underlying.clear()
    underlying addAll set
    underlying
  }

  def distinct: JList[A] = new JArrayList[A](new util.LinkedHashSet[A](underlying))

  def append(list: JList[A]): JList[A] = {
    underlying addAll list
    underlying
  }

  def apply(index: Int): A = {
    underlying.get(index)
  }

  def lastOption: Option[A] = if (underlying.isEmpty) None else Some(underlying.get(underlying.size - 1))
  def last: A = underlying.get(underlying.size - 1)
  def tail: JList[A] = {
    if (underlying.isEmpty) throw new UnsupportedOperationException("empty.tail")
    new JArrayList[A](underlying.subList(1, underlying.size()))
  }

  def sortInPlace(fun: (A,A) => Int, reverse: Boolean = false): JList[A] = {
    val mult = if (reverse) -1 else 1
    val comparator = new Comparator[A]() {
      def compare(o1: A, o2: A) = fun(o1, o2) * mult
    }
    Collections.sort(underlying, comparator)
    underlying
  }

  def grouped(size: Int): JList[JList[A]] = {
    val res = new util.ArrayList[JList[A]]()
    for (i <- 0 until (underlying.size(), size)) {
      res add new util.ArrayList(underlying.subList(i, math.min(i + size, underlying.size())))
    }
    res
  }

  def groupBy[B](fun: A => B): JMap[B, JList[A]] = {
    val res = new JHashMap[B, JList[A]]()
    val it = underlying.iterator
    while(it.hasNext) {
      val el = it.next()
      val key = fun(el)
      if (!res.containsKey(key)) res.put(key, new JArrayList[A]())
      res.get(key).add(el)
    }
    res
  }

  def take(cnt: Int): JArrayList[A] = {
    val it = underlying.iterator
    val res = new JArrayList[A]
    var i = 0
    while (i < cnt && it.hasNext) {
      res.add(it.next)
      i += 1
    }
    res
  }

  def dropWhile(fun: A => Boolean): JArrayList[A] = {
    val it = underlying.iterator
    val res = new JArrayList[A]
    var go = false
    while(it.hasNext) {
      val el = it.next()
      if (!fun(el)) go = true
      if (go) res.add(el)
    }
    res
  }

  def takeWhile(fun: A => Boolean): JArrayList[A] = {
    val it = underlying.iterator
    val res = new JArrayList[A]
    while(it.hasNext) {
      val el = it.next()
      if (!fun(el)) return res
      res.add(el)
    }
    res
  }

  def toSet: JSet[A] = new JHashSet[A](underlying)

  def splitAt(pos: Int): (JList[A], JList[A]) = {
    val l = new JArrayList[A](pos)
    val r = new JArrayList[A](math.max(underlying.size() - pos, 0))
    var i = 0
    val it = underlying.iterator
    while (it.hasNext) {
      val el = it.next()
      (if (i < pos) l else r).add(el)
      i += 1
    }
    (l, r)
  }
}