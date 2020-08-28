package xyz.hyperreal.matrix

object Main extends App {

  val a = Matrix[Double](List(List(-2, 2, -3), List(-1, 1, 3), List(2, 0, -1)))
  val b = Matrix[Double](List(List(1, -4), List(0, 3)))
//  val c = Matrix.col(11, 12, 13)
//  val d = Matrix.row(14, 15, 16)

  println(a)
  println(a.inv * a)
  println(b.inv * b)

}
