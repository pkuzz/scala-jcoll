package com.pkuzz.util.scala.jcollections

import java.lang.{Iterable => JIterable}
import java.util.{Iterator => JIterator, List => JList, Map => JMap, Set => JSet}

object Implicits {

  implicit def wrapIterator[A](col : JIterator[A]) = new JIteratorWrapper(col)
  implicit def wrapIterable[A](col : JIterable[A]) = new JIterableWrapper[A](col)
  implicit def wrapArrayList[A](list : JList[A]) = new JArrayListWrapper(list)
  implicit def wrapHashSet[A](set : JSet[A]) = new JHashSetWrapper(set)
  implicit def wrapHashMap[A, B](map : JMap[A, B]) = new JHashMapWrapper(map)
}
