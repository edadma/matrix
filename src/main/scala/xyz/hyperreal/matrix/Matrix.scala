package xyz.hyperreal.matrix

import scala.collection.immutable.{ArraySeq, IndexedSeq}
import xyz.hyperreal.table.TextTable

import scala.collection.mutable.ArrayBuffer

abstract class Matrix[F](implicit field: Fractional[F]) extends IndexedSeq[F] with ((Int, Int) => F) {

  val rows: Int
  val cols: Int

  def dim: (Int, Int) = (rows, cols)

  def length: Int = rows * cols

  def isRow: Boolean = rows == 1

  def isCol: Boolean = cols == 1

  def isVector: Boolean = isRow || isCol

  def elements: Seq[(Int, Int, F)] = for (i <- 1 to rows; j <- 1 to cols) yield (i, j, this(i, j))

  def isZero: Boolean = this forall (field.toDouble(_) == 0)

  def isDiagonal: Boolean = elements forall { case (i, j, v) => i == j || field.toDouble(v) == 0 }

  def isSquare: Boolean = rows == cols

//  def det = {
//    require(!isSquare, "Matrix.det: must be a square matrix")
//
//    if (rows == 2)
//      this(1, 1)
//  }

  def apply(idx: Int): F = apply(idx / cols + 1, idx % cols + 1)

  def concrete = new ConcreteMatrix(rows, cols, apply)

//  def prependCol(m: Matrix[F]): Matrix[F] = Matrix.horiz[F](m, this)
//
//  def appendCol(m: Matrix[F]): Matrix[F] = Matrix.horiz[F](this, m)

//  def prependRow(m: Matrix): Matrix = Matrix.horiz(m, this)
//
//  def appendRow(m: Matrix): Matrix = Matrix.horiz( this, m)

  def block(ridx: Int, height: Int, cidx: Int, width: Int): Matrix[F] = {
    require(1 <= ridx && ridx <= rows, s"Matrix.view: row out of range: $ridx")
    require(1 <= cidx && cidx <= cols, s"Matrix.view: col out of range: $cidx")
    require(1 <= height && height <= rows, s"Matrix.view: height out of range: $height")
    require(1 <= width && width <= cols, s"Matrix.view: width out of range: $width")

    val enclosing = this

    new Matrix {
      val rows: Int = height
      val cols: Int = width

      def apply(r: Int, c: Int): F = enclosing(r + ridx - 1, c + cidx - 1)
    }
  }

  def row(ridx: Int): Matrix[F] = block(ridx, 1, 1, cols)

  def col(cidx: Int): Matrix[F] = block(1, rows, cidx, 1)

  def operation(that: Matrix[F], name: String, op: (F, F) => F): Matrix[F] = {
    require(rows == that.rows && cols == that.cols, s"$name: operand matrices must be of equal dimension")
    new ConcreteMatrix(rows, cols, (i, j) => op(this(i, j), that(i, j)))
  }

  def transpose = new ConcreteMatrix(cols, rows, (i, j) => this(j, i))

  def scale(s: Number) = new ConcreteMatrix(rows, cols, (i, j) => this(i, j).doubleValue * s.doubleValue)

  def add(that: Matrix[F]): Matrix[F] = operation(that, "Matrix.add", _.doubleValue + _.doubleValue)

  def +(that: Matrix[F]): Matrix[F] = add(that)

  def sub(that: Matrix[F]): Matrix[F] = operation(that, "Matrix.sub", _.doubleValue - _.doubleValue)

  def -(that: Matrix[F]): Matrix[F] = sub(that)

  def prod(that: Matrix[F]): Number = {
    require(isVector && that.isVector, "Matrix.prod: operands must be vectors")
    require(length == that.length, "Matrix.prod: operands must be of equal length")
    this zip that map { case (a, b) => a.doubleValue * b.doubleValue } sum
  }

  def elemMul(that: Matrix[F]): Matrix[F] = operation(that, "Matrix.elemMul", _.doubleValue * _.doubleValue)

  def mul(that: Matrix[F]): Matrix[F] = {
    require(cols == that.rows, "Matrix.mul: width of left operand must equal height of right operand")
    new ConcreteMatrix(rows, that.cols, (i, j) => this.row(i) prod that.col(j))
  }

  def *(that: Matrix[F]): Matrix[F] = mul(that)

//  def show = {
//    for (i <- 1 to rows)
//      println((for (j <- 1 to cols) yield this(i, j)) mkString (" "))
//
//    println
//  }

  override def toString: String =
    new TextTable() {
      for (i <- 1 to rows)
        rowSeq(Matrix.this.row(i))
    }.toString

}

class ConcreteMatrix[F](val rows: Int, val cols: Int, init: (Int, Int) => F)(implicit field: Fractional[F])
    extends Matrix[F] {

  private val data = ArraySeq.tabulate[F](rows, cols)((i: Int, j: Int) => init(i + 1, j + 1))

  def apply(row: Int, col: Int): F = {
    require(0 < row && row <= rows, s"new Matrix: row out of range: $row")
    require(0 < col && col <= cols, s"new Matrix: column out of range: $col")
    data(row - 1)(col - 1)
  }

}

object Matrix {

  def apply[F](data: Seq[Seq[F]])(implicit field: Fractional[F]): Matrix[F] = {
    require(data.nonEmpty, "Matrix cannot be empty")
    require(data forall (_.nonEmpty), "Matrix cannot have an empty row")
    require(data forall (_.length == data.head.length), "Matrix must have rows of same length")
    new ConcreteMatrix[F](data.length, data.head.length, (x: Int, y: Int) => data(x - 1)(y - 1))
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

  def horiz(mat: Matrix, mats: Matrix*): Matrix = {
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
