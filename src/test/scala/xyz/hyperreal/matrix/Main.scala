package xyz.hyperreal.matrix

import xyz.hyperreal.numbers_jvm.Rational

object Main extends App {

  val a = Matrix[Rational](List(List(3, 2, -5), List(1, -3, 2), List(5, -1, 4)))
  val b = Matrix.col[Rational](12, -13, 10)

  println(a.transpose)
  println(a.inv)
  println(a.inv * b)

}
