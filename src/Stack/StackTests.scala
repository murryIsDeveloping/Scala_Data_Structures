package Stack

object StackTests extends App {
  val s1 = MyStack(10,11,12)
  val s2 = MyStack(1,2,3)
  val s3 = s1 ++ s2
  s3.forEach(println)
  println("Map")
  s3.map(_*2).forEach(println)
  println("Flatmap")
  s3.flatMap(x => MyStack(x+1, x+2, x+3)).forEach(println)
  println("Filter")
  s3.filter(_%2 != 0).forEach(println)
}
