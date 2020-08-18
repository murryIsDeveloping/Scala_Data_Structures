package Set

import scala.annotation.tailrec

abstract class MySet[T] {
  val isEmpty: Boolean
  def apply(value: T): Boolean = contains(value)
  def contains(value: T): Boolean
  def + (value: T): MySet[T]
  def ++ (values: MySet[T]): MySet[T]
  def - (value: T): MySet[T]
  def -- (values: MySet[T]): MySet[T]
//  def & (value: MySet[T]): MySet[T]
  def map[A](fn: T => A): MySet[A]
  def flatMap[A](fn: T => MySet[A]): MySet[A]
  def filter(fn: T => Boolean): MySet[T]
  def forEach(fn: T => Unit): Unit
  def unary_! : MySet[T]
}

class EmptySet[T] extends MySet[T] {
  val isEmpty = true
  def contains(value: T): Boolean = false
  def + (value: T): MySet[T] = new ValuesSet[T](value, this)
  def ++ (values: MySet[T]): MySet[T] = values
  def - (value: T): MySet[T] = this
  def -- (values: MySet[T]): MySet[T] = this
//  def & (value: MySet[T]): MySet[T] = this
  def map[A](fn: T => A): MySet[A] = new EmptySet[A]
  def flatMap[A](fn: T => MySet[A]): MySet[A] = new EmptySet[A]
  def filter(fn: T => Boolean): MySet[T] = this
  def forEach(fn: T => Unit): Unit = ()
  def unary_! : MySet[T] = new PredicateSet[T](_ => true)
}

class ValuesSet[T](head: T, tail: MySet[T]) extends MySet[T] {
  val isEmpty = false
  def contains(value: T): Boolean = if(head == value) true else tail.contains(value)
  def + (value: T): MySet[T] = if(contains(value)) this else new ValuesSet[T](value, this)
  def ++ (values: MySet[T]): MySet[T] = new ValuesSet[T](head, tail ++ values)
  def - (value: T): MySet[T] = filter(x => !contains(x))
  def -- (values: MySet[T]): MySet[T] = filter(x => values.contains(x))
//  def & (value: MySet[T]): MySet[T]
  def map[A](fn: T => A): MySet[A] = new ValuesSet[A](fn(head), tail.map(fn))
  def flatMap[A](fn: T => MySet[A]): MySet[A] = fn(head) ++ tail.flatMap(fn)
  def filter(fn: T => Boolean): MySet[T] = {
    val filteredTail = tail.filter(fn)
    if (fn(head)) new ValuesSet[T](head, filteredTail) else filteredTail
  }
  def forEach(fn: T => Unit): Unit = {
    fn(head)
    tail.forEach(fn)
  }

  def unary_! : MySet[T] = new PredicateSet[T](x => !contains(x))
}

class PredicateSet[T](predicateSet: T => Boolean) extends MySet[T] {
  val isEmpty = false
  def contains(value: T): Boolean = predicateSet(value)
  def + (value: T): MySet[T] = new PredicateSet[T](x => predicateSet(x) || x == value)
  def ++ (values: MySet[T]): MySet[T] = new PredicateSet[T](x => predicateSet(x) || values.contains(x))
  def - (value: T): MySet[T] = new PredicateSet[T](x => x != value && predicateSet(x))
  def -- (values: MySet[T]): MySet[T] = new PredicateSet[T](x => !values.contains(x) && predicateSet(x))
//  def & (value: MySet[T]): MySet[T]
  def map[A](fn: T => A): MySet[A] = throw new RuntimeException("Can't map a negated (infinite) set")
  def flatMap[A](fn: T => MySet[A]): MySet[A] = throw new RuntimeException("Can't map a negated (infinite) set")
  def filter(fn: T => Boolean): MySet[T] = new PredicateSet[T](x => fn(x) && predicateSet(x))
  def forEach(fn: T => Unit): Unit = throw new RuntimeException("Can't forEach on an negated (infinite) set")
  def unary_! : MySet[T] = new PredicateSet[T](x => !predicateSet(x))
}

object MySet {
  def apply[T](values: T*): MySet[T] = {
    @tailrec
    def helper(values: Seq[T], set: MySet[T]): MySet[T] = {
      if(values.isEmpty) set
      else helper(values.tail, set + values.head)
    }
    helper(values, new EmptySet[T])
  }
}