package xyz.hyperreal.matrix

import xyz.hyperreal.numbers_jvm.Rational

object Main extends App {

  val a = Matrix[Rational](List(List(-2, 2, -3), List(-1, 1, 3), List(2, 0, -1)))
  val b = Matrix[Rational](List(List(1, -4), List(0, 3)))

  println(a.det)
  println(a.inv * a)

}
