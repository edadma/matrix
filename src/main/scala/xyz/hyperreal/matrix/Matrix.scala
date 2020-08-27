package xyz.hyperreal.matrix

import scala.collection.immutable.{ArraySeq, IndexedSeq}

import xyz.hyperreal.table.TextTable

class Matrix(val rows: Int, val cols: Int, init: (Int, Int) => Number)
    extends IndexedSeq[Number]
    with ((Int, Int) => Number) {

  private val data = ArraySeq.tabulate[Number](rows, cols)((x: Int, y: Int) => init(x + 1, y + 1))

  def apply(row: Int, col: Int): Number = {
    require(0 < row && row <= rows, s"new Matrix: row out of range: $row")
    require(0 < col && col <= cols, s"new Matrix: column out of range: $col")
    data(row - 1)(col - 1)
  }

  def apply(idx: Int): Number = data(idx / cols)(idx % cols)

  val length: Int = rows * cols

  def row(r: Int) = new Matrix(1, cols, (_, c) => apply(r, c))

  def col(c: Int) = new Matrix(rows, 1, (r, _) => apply(r, c))

  def operation(that: Matrix, op: (Int, Int, Matrix, Matrix) => Number): Matrix =
    new Matrix(rows, cols, op(_, _, this, that))

  def elementWiseOperation(that: Matrix, name: String, op: (Number, Number) => Number): Matrix = {
    require(rows == that.rows && cols == that.cols, s"$name: operand matrices must be of equal dimension")

    operation(that, (i: Int, j: Int, a: Matrix, b: Matrix) => op(a(i, j), b(i, j)))
  }

  def add(that: Matrix): Matrix =
    elementWiseOperation(that, "Matrix.add", (a: Number, b: Number) => a.doubleValue + b.doubleValue)

  def +(that: Matrix): Matrix = add(that)

  def sub(that: Matrix): Matrix =
    elementWiseOperation(that, "Matrix.sub", (a: Number, b: Number) => a.doubleValue - b.doubleValue)

  def -(that: Matrix): Matrix = sub(that)

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
