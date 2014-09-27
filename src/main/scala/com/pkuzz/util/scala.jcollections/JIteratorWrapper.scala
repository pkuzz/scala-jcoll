package com.pkuzz.util.scala.jcollections

import java.util.{Iterator => JIterator}

class JIteratorWrapper[A](underlying: JIterator[A]) {
  def foreach(fun: A => Unit): Unit = {
    while(underlying.hasNext) fun(underlying.next())
  }
}
