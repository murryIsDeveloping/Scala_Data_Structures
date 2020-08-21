package Stream

object MyStreamTest extends App {
    val naturals = MyStream.from(2)(_ + 1)
    //  println(naturals.head)
    //  println(naturals.tail.head)
    //  println(naturals.tail.tail.head)
    //
    //  val startFrom0 = 0 #:: naturals
    //  println(startFrom0.head)


    //  startFrom0.filter(_ % 3 == 0).take(100).foreach(println)
    //  startFrom0.map(_ * 3).take(100).foreach(println)
    //  startFrom0.flatMap(x => new ValueStream(x, new ValueStream(x + 1, Empty))).take(100).foreach(println)
    //  startFrom0.filter(_ < 10).toList()

    //  val fib = MyStream.from(Seq(0,1))(s => Seq(s(1), s(0) + s(1)))
    //  fib.map(x => x(0)).take(10).foreach(println)

    def fibonacci(first: Int, second: Int): MyStream[Int] = new ValueStream[Int](first, fibonacci(second, first + second))

    def prime(numbers: MyStream[Int]) : MyStream[Int] =
      if(numbers.isEmpty) numbers
      else new ValueStream[Int](numbers.head, prime(numbers.tail).filter(_ % numbers.head != 0))

    prime(naturals).take(100).foreach(println)
}
