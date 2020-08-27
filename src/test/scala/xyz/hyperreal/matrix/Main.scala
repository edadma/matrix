package xyz.hyperreal.matrix

object Main extends App {

  val a = Matrix[Double](List(List(1, 2, 3), List(4, 5, 6)))
  val b = Matrix[Double](List(List(10, 11), List(20, 21), List(30, 31)))
//  val c = Matrix.col(11, 12, 13)
//  val d = Matrix.row(14, 15, 16)

  println(a * b)

}
