package xyz.hyperreal.matrix

object Main extends App {

  val a = Matrix(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
  val b = Matrix(List(List(10, 20, 30), List(4, 5, 6)))
  val c = Matrix.col(11, 12, 13)
  val d = Matrix.row(14, 15, 16)

  println(a.appendCol(c))

}
