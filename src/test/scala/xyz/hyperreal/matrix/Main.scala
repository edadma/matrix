package xyz.hyperreal.matrix

import xyz.hyperreal.dal.NumberIsFractional._

import math.Fractional.Implicits._

object Main extends App {

  val a = Matrix[Number](List(List(3, 2, -5), List(1, -3, 2), List(5, -1, 4)))
//  val b = Matrix[Rational](List(List(2, 5, 0, 8), List(1, 4, 2, 6), List(7, 8, 9, 3), List(1, 5, 7, 8)))
//  val c = Matrix.fromRows[Rational](2, 1, 2, 3, 4, 5, 6)
//  val d = Matrix.fromSeq[Rational](2, c)

  println(a)
//  println(d)

  println(a / 2)
  println(a.inv)
  println(a.inv * a)

}
