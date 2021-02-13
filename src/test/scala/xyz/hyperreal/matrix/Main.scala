package xyz.hyperreal.matrix

import xyz.hyperreal.dal.NumberIsFractional._

import math.Fractional.Implicits._

object Main extends App {

//  val a = Matrix[Number](List(List(3, 2, -5), List(1, -3, 2), List(5, -1, 4)))
//
//  println(a)
//
//  println(a / 2)
//  println(a.inv)
//  println(a.inv * a)

  val a = Matrix[Number](List(List(1, 2, 1), List(-2, -3, 1), List(3, 5, 0)))

  println(a)
  println(a.rowEchelonForm)
  println(a.reducedRowEchelonForm)
  println(a.rank)

}
