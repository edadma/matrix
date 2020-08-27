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

  def operation(that: Matrix, name: String, op: (Number, Number) => Number): Matrix = {
    require(rows == that.rows && cols == that.cols, s"$name: operand matrices must be of equal dimension")
    new Matrix(rows, cols, (i, j) => op(this(i, j), that(i, j)))
  }

  def add(that: Matrix): Matrix = operation(that, "Matrix.add", _.doubleValue + _.doubleValue)

  def +(that: Matrix): Matrix = add(that)

  def sub(that: Matrix): Matrix = operation(that, "Matrix.sub", _.doubleValue - _.doubleValue)

  def -(that: Matrix): Matrix = sub(that)

  def prod(that: Matrix): Number = {
    require(rows == 1, "Matrix.prod: left operand should be row vector")
    require(cols == that.rows, "Matrix.prod: width of left operand must equal height of right operand")
    require(that.cols == 1, "Matrix.prod: right operand should be column vector")
    this zip that map { case (a, b) => a.doubleValue * b.doubleValue } sum
  }

  def elemMul(that: Matrix): Matrix = operation(that, "Matrix.elemMul", _.doubleValue * _.doubleValue)

  def mul(that: Matrix): Matrix = {
    require(cols == that.rows, "Matrix.mul: width of left operand must equal height of right operand")
    new Matrix(rows, that.cols, (i, j) => this.row(i) prod that.col(j))
  }

  def *(that: Matrix): Matrix = mul(that)

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

  def diagonal(size: Int, value: Number) = new Matrix(size, size, (i, j) => if (i == j) value else 0)

  def identity(size: Int): Matrix = diagonal(size, 1)

  def row(elems: Number*): Matrix = {
    require(elems.nonEmpty, "Matrix.row: need at least one element")
    Matrix(List(elems))
  }

  def col(elems: Number*): Matrix = {
    require(elems.nonEmpty, "Matrix.col: need at least one element")
    new Matrix(elems.length, 1, (r, _) => elems(r))
  }

}
