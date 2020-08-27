package xyz.hyperreal.matrix

import scala.collection.immutable.{ArraySeq, IndexedSeq}
import xyz.hyperreal.table.TextTable

import scala.collection.mutable.ArrayBuffer

abstract class Matrix extends IndexedSeq[Number] with ((Int, Int) => Number) {

  val rows: Int
  val cols: Int

  def length: Int = rows * cols

  def row(r: Int) = new ConcreteMatrix(1, cols, (_, c) => apply(r, c))

  def col(c: Int) = new ConcreteMatrix(rows, 1, (r, _) => apply(r, c))

  override def toString: String =
    new TextTable() {
      for (i <- 1 to rows)
        rowSeq(Matrix.this.row(i))
    }.toString

}

class ConcreteMatrix(val rows: Int, val cols: Int, init: (Int, Int) => Number) extends Matrix {

  private val data = ArraySeq.tabulate[Number](rows, cols)((i: Int, j: Int) => init(i + 1, j + 1))

  def apply(row: Int, col: Int): Number = {
    require(0 < row && row <= rows, s"new Matrix: row out of range: $row")
    require(0 < col && col <= cols, s"new Matrix: column out of range: $col")
    data(row - 1)(col - 1)
  }

  def view(ridx: Int, height: Int, cidx: Int, width: Int): Matrix = {
    require(1 <= ridx && ridx <= rows, s"Matrix.view: row out of range: $ridx")
    require(1 <= cidx && cidx <= rows, s"Matrix.view: col out of range: $cidx")
    require(1 <= height && height <= rows, s"Matrix.view: height out of range: $height")
    require(1 <= width && width <= cols, s"Matrix.view: width out of range: $width")
    new Matrix {
      val rows: Int = width
      val cols: Int = height

      def apply(idx: Int): Number = apply(idx / cols + 1, idx % cols + 1)

      def apply(r: Int, c: Int): Number = ConcreteMatrix.this(r + ridx - 1, c + cidx - 1)
    }
  }

  def apply(idx: Int): Number = data(idx / cols)(idx % cols)

  val dim: (Int, Int) = (rows, cols)

  def operation(that: Matrix, name: String, op: (Number, Number) => Number): Matrix = {
    require(rows == that.rows && cols == that.cols, s"$name: operand matrices must be of equal dimension")
    new ConcreteMatrix(rows, cols, (i, j) => op(this(i, j), that(i, j)))
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
    new ConcreteMatrix(rows, that.cols, (i, j) => this.row(i) prod that.col(j))
  }

  def *(that: Matrix): Matrix = mul(that)

}

object Matrix {

  def apply(data: Seq[Seq[Number]]): Matrix = {
    require(data.nonEmpty, "Matrix cannot be empty")
    require(data forall (_.nonEmpty), "Matrix cannot have an empty row")
    require(data forall (_.length == data.head.length), "Matrix must have rows of same length")
    new ConcreteMatrix(data.length, data.head.length, (x: Int, y: Int) => data(x - 1)(y - 1))
  }

  def diagonal(size: Int, value: Number) = new ConcreteMatrix(size, size, (i, j) => if (i == j) value else 0)

  def identity(size: Int): Matrix = diagonal(size, 1)

  def row(elems: Number*): Matrix = {
    require(elems.nonEmpty, "Matrix.row: need at least one element")
    Matrix(List(elems))
  }

  def col(elems: Number*): Matrix = {
    require(elems.nonEmpty, "Matrix.col: need at least one element")
    new ConcreteMatrix(elems.length, 1, (r, _) => elems(r - 1))
  }

  def cath(mat: Matrix, mats: Matrix*): Matrix = {
    require(mats.forall(m => m.rows == mat.rows),
            "Matrix.cath: matrices being horizontally concatenated must have same height")

    val ms = new ArrayBuffer[(Matrix, Int)]
    var rcols = 0

    for (m <- mat +: mats) {
      for (i <- 1 to m.cols)
        ms append ((m, i))

      rcols += m.cols
    }

    new ConcreteMatrix(mat.rows, rcols, (i, j) => ms(j - 1)._1(i, ms(j - 1)._2))
  }

}
