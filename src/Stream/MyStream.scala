package Stream

  import scala.annotation.tailrec

  abstract class MyStream[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    def #::[B >: A](element: B): MyStream[B]
    def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B]

    def foreach(f: A => Unit): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(f: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A]
    def takeAsList(n: Int): List[A] = take(n).toList()

    @tailrec
    final def toList[B >: A](acc: List[B] = Nil): List[B] = {
      if (isEmpty) acc.reverse
      else tail.toList(head :: acc)
    }
  }

  case object Empty extends MyStream[Nothing] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException
    def tail: MyStream[Nothing] = throw new NoSuchElementException

    def #::[B >: Nothing](element: B): MyStream[B] = new ValueStream[B](element, this)
    def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

    def foreach(f: Nothing => Unit): Unit = ()
    def map[B](f: Nothing => B): MyStream[B] = this
    def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
    def filter(f: Nothing => Boolean): MyStream[Nothing] = this

    def take(n: Int): MyStream[Nothing] = this
  }

  class ValueStream[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
    def isEmpty: Boolean = false

    override val head: A = hd
    override lazy val tail: MyStream[A] = tl

    def #::[B >: A](element: B): MyStream[B] = new ValueStream[B](element, this)
    def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new ValueStream[B](head, tail ++ anotherStream)

    def foreach(f: A => Unit): Unit = {
      f(head)
      tail.foreach(f)
    }
    def map[B](f: A => B): MyStream[B] = new ValueStream[B](f(head), tail.map(f))
    def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)
    def filter(f: A => Boolean): MyStream[A] = if (f(head)) new ValueStream[A](head, tail.filter(f)) else tail.filter(f)

    def take(n: Int): MyStream[A] = {
      if(n <= 0) Empty
      else if (n == 1) new ValueStream[A](head, Empty)
      else new ValueStream[A](head, tail.take(n-1))
    }
  }

  object MyStream {
    def from[A](start: A)(generator: A => A): MyStream[A] = new ValueStream(start, MyStream.from(generator(start))(generator))
  }
