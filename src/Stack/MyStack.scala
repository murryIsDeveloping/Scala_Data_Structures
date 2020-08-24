package Stack

abstract class MyStack[+T] {
  val isEmpty: Boolean
  def + [B >: T](element: B): MyStack[B]
  def ++ [B >: T](elements: MyStack[B]): MyStack[B]
  def next() : (T, MyStack[T])
  def map[B >: T](fn: T => B): MyStack[B]
  def flatMap[B >: T](fn: T => MyStack[B]): MyStack[B]
  def filter(fn: T => Boolean): MyStack[T]
  def forEach(fn: T => Unit): Unit
}

case object EmptyStack extends MyStack[Nothing] {
  val isEmpty: Boolean = true
  def +[B >: Nothing](element: B): MyStack[B] = new MyStackCons[B](element, this)
  def ++[B >: Nothing](elements: MyStack[B]): MyStack[B] = elements
  def next(): (Nothing, MyStack[Nothing]) = throw new NoSuchFieldError()
  def map[B >: Nothing](fn: Nothing => B): MyStack[B] = this
  def flatMap[B >: Nothing](fn: Nothing => MyStack[B]): MyStack[B] = this
  def filter(fn: Nothing => Boolean): MyStack[Nothing] = this
  def forEach(fn: Nothing => Unit): Unit = ()
}

class MyStackCons[T](val head: T, val tail: MyStack[T]) extends MyStack[T] {
  val isEmpty: Boolean = false
  def +[B >: T](element: B): MyStack[B] = new MyStackCons[B](element, this)
  def ++[B >: T](elements: MyStack[B]): MyStack[B] = {
    if (elements.isEmpty) this
    else {
      val (h, t) = elements.next()
      new MyStackCons[B](h, this ++ t)
    }
  }
  def next(): (T, MyStack[T]) = (head, tail)
  def map[B >: T](fn: T => B): MyStack[B] = new MyStackCons[B](fn(head), tail.map(fn))
  def flatMap[B >: T](fn: T => MyStack[B]): MyStack[B] = tail.flatMap(fn) ++ fn(head)
  def filter(fn: T => Boolean): MyStack[T] = if (fn(head)) new MyStackCons[T](head, tail.filter(fn)) else tail.filter(fn)
  def forEach(fn: T => Unit): Unit = {
    fn(head)
    tail.forEach(fn)
  }
}

object MyStack {
  def apply[T](elements: T*): MyStack[T] = {
    def helper(myStack: MyStack[T], elements: Seq[T]): MyStack[T] ={
      if(elements.isEmpty) myStack
      else helper(myStack + elements.head, elements.tail)
    }

    helper(EmptyStack, elements)
  }
}