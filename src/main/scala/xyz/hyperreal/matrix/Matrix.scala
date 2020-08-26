package xyz.hyperreal.matrix

import scala.collection.immutable.ArraySeq

import xyz.hyperreal.table.TextTable

class Matrix(val rows: Int, val cols: Int, init: (Int, Int) => Number) {

  private val data = ArraySeq.tabulate[Number](rows, cols)((x: Int, y: Int) => init(x + 1, y + 1))

  def apply(row: Int, col: Int): Number = {
    require(0 < row && row <= rows, s"Matrix: row out of range: $row")
    require(0 < col && col <= cols, s"Matrix: row out of range: $col")
    data(row - 1)(col - 1)
  }

  def operation(that: Matrix, op: (Int, Int, Matrix, Matrix) => Number): Matrix =
    new Matrix(rows, cols, op(_, _, this, that))

  def element(that: Matrix, name: String, op: (Int, Int, Matrix, Matrix) => Number): Matrix = {
    require(rows == that.rows && cols == that.cols, s"Matrix.$name: only matrices of equal dimension can be added")

    operation(that, op)
  }

  def add(that: Matrix): Matrix =
    element(that, "add", (i: Int, j: Int, a: Matrix, b: Matrix) => a(i, j).doubleValue + b(i, j).doubleValue)

  def +(that: Matrix): Matrix = add(that)

  override def toString: String =
    new TextTable() {
      data foreach { r =>
        rowSeq(r)
      }
    }.toString
}

object Matrix {

  def apply(data: Seq[Seq[Number]]): Matrix = {
    require(data.nonEmpty, "Matrix cannot be empty")
    require(data forall (_.nonEmpty), "Matrix cannot have an empty row")
    require(data forall (_.length == data.head.length), "Matrix must have rows of same length")

    new Matrix(data.length, data.head.length, (x: Int, y: Int) => data(x - 1)(y - 1))
  }

}
