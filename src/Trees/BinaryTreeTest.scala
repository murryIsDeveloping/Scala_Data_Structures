package Trees

object BinaryTreeTest extends App {
  val aTree = BinaryTree[Int]() + 10 + 2 + 11 + 19 + 14
  val bTree = BinaryTree[Int]() + 5 + 2 + 3 + 1 + 6 + 8 +16
  val cTree = (aTree ++ bTree) - 10
  println(cTree.filter(_%2 == 0).map[String](x => s"$x").asSeq())
}
