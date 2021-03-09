package xyz.hyperreal.matrix

import xyz.hyperreal.numbers.ComplexDouble
import xyz.hyperreal.numbers.ComplexDoubleIsFractional._

import math.Fractional.Implicits._

object Main extends App {

//  val a = Matrix[Number](List(List(3, 2, -5), List(1, -3, 2), List(5, -1, 4)))
//
//  println(a)
//  println(a / 2)
//  println(a.inverse)
//  println(a.inverse * a)

//  val a = Matrix[Number](List(List(1, 2, 1), List(-2, -3, 1), List(3, 5, 0)))
//
//  println(a)
//  println(a.rowEchelonForm)
//  println(a.reducedRowEchelonForm)
//  println(a.rank)
//  println(a.solution)
//
//  println(Matrix.zero(3).rank)

//  val a = Matrix[Number](List(List(2, 3, 1, 5), List(6, 13, 5, 19), List(2, 19, 10, 23), List(4, 10, 11, 31)))
//  val (l, u, p) = a.LUP
//
////  println(a)
////  println(l)
////  println(u)
////  println(p)
//  println(p * a == l * u)

//  val a = Matrix[Double](List(List(2, 0, 2, .6), List(3, 3, 4, -2), List(5, 5, 4, 2), List(-1, -2, 3.4, -1)))
//  val (l, u, p) = a.LUP
//
//  println(a)
//  println(l * u)
//  println(p * a)
//  println(p * a == l * u)

  val a = Matrix[ComplexDouble](List(1, 2), List(3, 4))

  println(a)

}
