package List

import scala.annotation.tailrec

abstract class MyList[+T] {
  val isEmpty: Boolean
  val length: Int
  def apply(index: Int): T
  def + [B >: T](elem: B): MyList[B]
  def ++ [B >: T](elems: MyList[B]): MyList[B]
  def - [B >: T](elem: B): MyList[T]
  def map[B >: T](fn: T => B): MyList[B]
  def flatMap[B >: T](fn: T => MyList[B]): MyList[B]
  def filter(predicate: T => Boolean): MyList[T]
  def forEach(fn: T => Unit): Unit
  def reverse() : MyList[T]
  def sort(fn: (T,T) => Int): MyList[T]
}

case object EmptyList extends MyList[Nothing] {
  val isEmpty: Boolean = true
  val length = 0
  def apply(index: Int): Nothing = throw new IndexOutOfBoundsException()
  def +[B >: Nothing](elem: B): MyList[B] = new ValueList[B](elem, this)
  def ++[B >: Nothing](elems: MyList[B]): MyList[B] = elems
  def -[B >: Nothing](elem: B): MyList[Nothing] = this
  def flatMap[B >: Nothing](fn: Nothing => MyList[B]): MyList[B] = this
  def map[B >: Nothing](fn: Nothing => B): MyList[B] = this
  def filter(predicate: Nothing => Boolean): MyList[Nothing] = this
  def forEach(fn: Nothing => Unit): Unit = ()
  def reverse(): MyList[Nothing] = this
  def sort(fn: (Nothing,Nothing) => Int): MyList[Nothing] = this
}

class ValueList[T](head: T, tail: MyList[T]) extends MyList[T] {
  val isEmpty: Boolean = false
  val length: Int = 1 + tail.length
  def apply(index: Int): T =
    if(index < 0) throw new IndexOutOfBoundsException()
    else if(index == 0) head
    else tail(index-1)
  def +[B >: T](elem: B): MyList[B] = new ValueList(head, tail + elem)
  def ++[B >: T](elems: MyList[B]): MyList[B] = new ValueList[B](head, tail ++ elems)
  def -[B >: T](elem: B): MyList[T] = if(head == elem) tail else new ValueList(head, tail - elem)
  def map[B >: T](fn: T => B): MyList[B] = new ValueList(fn(head), tail.map(fn))
  def flatMap[B >: T](fn: T => MyList[B]): MyList[B] = fn(head) ++ tail.flatMap(fn)
  def filter(predicate: T => Boolean): MyList[T] = {
    val filteredTail = tail.filter(predicate)
    if (predicate(head)) new ValueList(head, filteredTail) else filteredTail
  }
  def forEach(fn: T => Unit): Unit = {
    fn(head)
    tail.forEach(fn)
  }
  def reverse() : MyList[T] = tail.reverse() + head

  def sort(fn: (T,T) => Int): MyList[T] = {
    if(tail.isEmpty) this
    else if(fn(head, apply(1)) >= 0) (tail + head).sort(fn)
    else new ValueList[T](head, tail.sort(fn))
  }
}

object MyList {
  def apply[T]() : MyList[T] = EmptyList
  def apply[T](elems: T*): MyList[T] = {
    @tailrec
    def applyHelper(elements: Seq[T], list: MyList[T]): MyList[T] = {
      if (elements.isEmpty) list else applyHelper(elements.tail, list + elements.head)
    }
    applyHelper(elems, EmptyList)
  }
}