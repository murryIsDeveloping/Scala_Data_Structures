package List

import scala.annotation.tailrec

abstract class MyList[T] {
  val isEmpty: Boolean
  val length: Int
  val head: T
  val tail: MyList[T]
  def apply(index: Int): T
  def + (elem: T): MyList[T]
  def ++ (elems: MyList[T]): MyList[T]
  def - (elem: T): MyList[T]
  def map[B](fn: T => B): MyList[B]
  def flatMap[B](fn: T => MyList[B]): MyList[B]
  def filter(predicate: T => Boolean): MyList[T]
  def forEach(fn: T => Unit): Unit
  def reverse() : MyList[T]
}

class EmptyList[T] extends MyList[T] {
  val isEmpty: Boolean = true
  val length = 0
  val head: T = throw new NoSuchElementException()
  val tail: MyList[T] = throw new NoSuchElementException()
  def apply(index: Int): T = throw new IndexOutOfBoundsException()
  def + (elem: T): MyList[T] = new ValueList[T](elem, this)
  def ++ (elems: MyList[T]): MyList[T] = elems
  def - (elem: T): MyList[T] = this
  def map[B](fn: T => B) = new EmptyList[B]
  def flatMap[B](fn: T => MyList[B]) = new EmptyList[B]
  def filter(predicate: T => Boolean): MyList[T] = this
  def forEach(fn: T => Unit): Unit = ()
  def reverse() : MyList[T] = this
}

class ValueList[T](val head: T, val tail: MyList[T]) extends MyList[T] {
  val isEmpty: Boolean = false
  val length: Int = 1 + tail.length
  def apply(index: Int): T =
    if(index < 0) throw new IndexOutOfBoundsException()
    else if(index == 0) head
    else tail(index-1)
  def + (elem: T): MyList[T]  = new ValueList(head, tail + elem)
  def ++ (elems: MyList[T]): MyList[T] = new ValueList[T](head, tail ++ elems)
  def - (elem: T): MyList[T] = if(head == elem) tail else new ValueList(head, tail - elem)
  def map[B](fn: T => B): MyList[B] = new ValueList(fn(head), tail.map(fn))
  def flatMap[B](fn: T => MyList[B]): MyList[B] = fn(head) ++ tail.flatMap(fn)
  def filter(predicate: T => Boolean): MyList[T] = {
    val filteredTail = tail.filter(predicate)
    if (predicate(head)) new ValueList(head, filteredTail) else filteredTail
  }
  def forEach(fn: T => Unit): Unit = {
    fn(head)
    tail.forEach(fn)
  }
  def reverse() : MyList[T] = tail.reverse() + head
}

object MyList {
  def apply[T]() : MyList[T] = new EmptyList[T]
  def apply[T](elems: T*): MyList[T] = {
    @tailrec
    def applyHelper(elements: Seq[T], list: MyList[T]): MyList[T] = {
      if (elements.isEmpty) list else applyHelper(elements.tail, list + elements.head)
    }
    applyHelper(elems, new EmptyList[T])
  }
}