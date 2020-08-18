package List

object MyListTests extends App {
  val list: MyList[Int] = MyList(1,2,3)
  val altList: MyList[Int] = MyList(10,20,30)
  println("Empty: " + MyList().isEmpty)
  println("Empty: " + list.isEmpty)
  println("Length: " + list.length)
  println("APPLY")
  println(list(1))
  println("ADDING")
  (list + 4).forEach(println)
  println("ADDING LISTS")
  (list ++ altList).forEach(println)
  println("MINUS ELEM")
  (list - 3).forEach(println)
  println("MAP")
  list.map(_*2).forEach(println)
  println("FLATMAP")
  list.flatMap(x => MyList(x, x+1, x+2)).forEach(println)
  println("FILTER")
  list.filter(_%2 == 0).forEach(println)
  println("REVERSE")
  list.reverse().forEach(println)


}