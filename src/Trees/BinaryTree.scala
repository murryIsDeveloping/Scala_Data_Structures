package Trees

object BinaryTree {
  def apply[T]()(implicit comparator: BinaryTreeSortOpts.BinaryTreeComparator[T]) : EmptyTreeCons[T] = {
    new EmptyTreeCons[T]()
  }
}

object BinaryTreeSortOpts {
  trait BinaryTreeComparator[T] {
    def apply(a: T, b: T): Boolean
  }

  implicit object AnyComparator extends BinaryTreeComparator[Any]{
    def apply(a: Any, b: Any): Boolean = throw new NoSuchMethodError("Implicit 'BinaryTreeComparator' function not implement for this type")
  }

  implicit object IntComparator extends BinaryTreeComparator[Int]{
    def apply(a: Int, b: Int): Boolean = a > b
  }

  implicit object StringComparator extends BinaryTreeComparator[String]{

    def apply(a: String, b: String): Boolean = a.compareTo(b) > 0
  }
}

abstract class BinaryTree[T] {
  val isEmpty: Boolean
  val left: BinaryTree[T]
  val right: BinaryTree[T]
  def value(): T
  def + (element: T): BinaryTree[T]
  def ++ (elements: BinaryTree[T]): BinaryTree[T]
  def print(): String
  def smallest(): T
  def largest(): T
  def asSeq(): Seq[T]
  def contains(element: T): Boolean
  def map[B](fn: T => B)(implicit comparator: BinaryTreeSortOpts.BinaryTreeComparator[B]): BinaryTree[B]
  def flatMap[B](fn: T => BinaryTree[B])(implicit comparator: BinaryTreeSortOpts.BinaryTreeComparator[B]): BinaryTree[B]
  def filter(fn: T => Boolean): BinaryTree[T]
  def - (element: Int): BinaryTree[T]
}

class BinaryTreeCons[T](v: T, val left: BinaryTree[T], val right: BinaryTree[T])(implicit comparator: BinaryTreeSortOpts.BinaryTreeComparator[T]) extends BinaryTree[T] {

  val isEmpty = false
  def value(): T = v

  def contains(element: T): Boolean = {
    if (element == value) true
    else if (comparator(value(), element)) left.contains(element)
    else right.contains(element)
  }

  def +(element: T): BinaryTree[T] = {
    if (element == value()) this
    else if (comparator(value(), element)) new BinaryTreeCons(value(), left + element, right)
    else new BinaryTreeCons(value(), left, right + element)
  }

  def ++ (elements: BinaryTree[T]): BinaryTree[T] = {
    if(elements.isEmpty) this
    else if (comparator(value(), elements.value())) new BinaryTreeCons(value(), left + elements.value, right) ++ elements.right ++ elements.left
    else new BinaryTreeCons(value(), left, right + elements.value) ++ elements.right ++ elements.left
  }

  def -(element: Int): BinaryTree[T] = {
    if(element == value()) right ++ left
    else (right - element) ++ (left - element)
  }

  def print(): String = s"${value()} [${left.print()} ${right.print()}]"

  def smallest(): T = if (left.isEmpty) value() else left.smallest()

  def largest(): T = if (right.isEmpty) value() else right.largest()

  def asSeq(): Seq[T] = (left.asSeq() :+ value) ++ right.asSeq()

  def map[B](fn: T => B)(implicit comparator: BinaryTreeSortOpts.BinaryTreeComparator[B]): BinaryTree[B] =
    left.map(fn) + fn(value()) ++ right.map(fn)

  def flatMap[B](fn: T => BinaryTree[B])(implicit comparator: BinaryTreeSortOpts.BinaryTreeComparator[B]): BinaryTree[B] =
    left.flatMap(fn) ++ fn(value())  ++ right.flatMap(fn)

  def filter(fn: T => Boolean): BinaryTree[T] =
    if(fn(value())) new BinaryTreeCons(value(), left.filter(fn), right.filter(fn))
    else left.filter(fn) ++ right.filter(fn)
}

class EmptyTreeCons[T](implicit comparator: BinaryTreeSortOpts.BinaryTreeComparator[T]) extends BinaryTree[T] {
  val isEmpty = true
  val left: BinaryTree[T] = this
  val right: BinaryTree[T]  = this
  def value(): T = throw new NoSuchFieldError()

  def contains(element: T): Boolean = false
  def + (element: T): BinaryTree[T] = new BinaryTreeCons[T](element, new EmptyTreeCons[T](), new EmptyTreeCons[T]())
  def ++ (elements: BinaryTree[T]): BinaryTree[T] = elements
  def -(element: Int): BinaryTree[T] = this

  def print(): String = ""
  def smallest() = throw new NoSuchMethodError()
  def largest() = throw new NoSuchMethodError()
  def asSeq(): Seq[Nothing] = Seq()

  def map[B](fn: T => B)(implicit comparator: BinaryTreeSortOpts.BinaryTreeComparator[B]): BinaryTree[B] = new EmptyTreeCons[B]()
  def flatMap[B](fn: T => BinaryTree[B])(implicit comparator: BinaryTreeSortOpts.BinaryTreeComparator[B]): BinaryTree[B] = new EmptyTreeCons[B]()
  def filter(fn: T => Boolean): BinaryTree[T] = this
}