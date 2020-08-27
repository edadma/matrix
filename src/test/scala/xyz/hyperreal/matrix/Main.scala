package xyz.hyperreal.matrix

object Main extends App {

  val a = Matrix(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
  val b = Matrix(List(List(10, 20, 30), List(4, 5, 6)))

  println(a * Matrix.diagonal(3, 2))

}
