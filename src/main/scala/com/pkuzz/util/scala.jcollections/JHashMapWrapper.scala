package com.pkuzz.util.scala.jcollections

import java.util.{Map => JMap,  HashMap => JHashMap}

class JHashMapWrapper[A,B](underlying: JMap[A,B]) {

  def apply(key: A) = {
    underlying.get(key)
  }

  def foreach(fun: (A, B) => Unit): Unit = {
    val it = underlying.entrySet.iterator
    while(it.hasNext) {
      val el = it.next()
      fun(el.getKey, el.getValue)
    }
  }

  def map[C, D](fun: (A, B) => (C, D)): JHashMap[C, D] = {
    val it = underlying.entrySet.iterator
    val res = new JHashMap[C, D](underlying.size)
    while(it.hasNext) {
      val el = it.next()
      val (key, value) = fun(el.getKey, el.getValue)
      res.put(key, value)
    }
    res
  }

  def getOrElse(key: A, default: B) = if (underlying.containsKey(key)) underlying.get(key) else default
}