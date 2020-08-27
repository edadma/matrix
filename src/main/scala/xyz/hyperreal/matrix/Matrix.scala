package xyz.hyperreal.matrix

import scala.collection.immutable.{IndexedSeq, AbstractSeq, ArraySeq}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import math.Fractional.Implicits._

import xyz.hyperreal.table.TextTable

abstract class Matrix[F](implicit field: Fractional[F])
    extends AbstractSeq[F]
    with IndexedSeq[F]
    with ((Int, Int) => F) {

  val rows: Int
  val cols: Int

  def elem(r: Int, c: Int): F

  protected def boundsCheck(r: Int, c: Int, name: String): Unit = {
    require(1 <= r && r <= rows, s"Matrix.$name: row out of range: $r")
    require(1 <= c && c <= cols, s"Matrix.$name: column out of range: $c")
  }

  def dim: (Int, Int) = (rows, cols)

  def length: Int = rows * cols

  def isRow: Boolean = rows == 1

  def isCol: Boolean = cols == 1

  def isVector: Boolean = isRow || isCol

  def elements: Seq[(Int, Int, F)] = for (i <- 1 to rows; j <- 1 to cols) yield (i, j, this(i, j))

  def isZero: Boolean = this forall (_ == field.zero)

  def isDiagonal: Boolean = elements forall { case (i, j, v) => i == j || v == field.zero }

  def isSquare: Boolean = rows == cols

  def minor(i: Int, j: Int): F = {
    boundsCheck(i, j, s"minor")
    withoutRow(i).withoutCol(j).det
  }

  def cofactor(i: Int, j: Int): F = {
    boundsCheck(i, j, s"minor")
    if ((i + j) % 2 == 1) -minor(i, j) else minor(i, j)
  }

  def det: F = {
    require(isSquare, "Matrix.det: must be a square matrix")

    rows match {
      case 1 => elem(1, 1)
      case 2 => elem(1, 1) * elem(2, 2) - elem(1, 2) * elem(2, 1)
      case _ => 1 to rows map (i => elem(i, 1) * cofactor(i, 1)) sum
    }
  }

  def inv(implicit t: ClassTag[F]): Matrix[F] = {
    require(isSquare, "Matrix.inv: must be a square matrix")

    Matrix.build(rows, cols, (i, j) => cofactor(j, i)).scale(field.one / det)
  }

  def apply(r: Int, c: Int): F = {
    boundsCheck(r, c, "apply")
    elem(r, c)
  }

  def apply(idx: Int): F = {
    require(0 <= idx && idx < length, s"Matrix (as Seq).apply: index out of range: $idx")
    elem(idx / cols + 1, idx % cols + 1)
  }

//  def concrete = new ConcreteMatrix(rows, cols, apply)

  def prependCol(m: Matrix[F])(implicit t: ClassTag[F]): Matrix[F] = Matrix.cath[F](m, this)

  def appendCol(m: Matrix[F])(implicit t: ClassTag[F]): Matrix[F] = Matrix.cath[F](this, m)

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

      def elem(r: Int, c: Int): F = enclosing.elem(r + ridx - 1, c + cidx - 1)
    }
  }

  def withoutRow(ridx: Int): Matrix[F] = {
    require(1 <= ridx && ridx <= rows, s"Matrix.withoutRow: row out of range: $ridx")

    val enclosing = this

    new Matrix {
      val rows: Int = enclosing.rows - 1
      val cols: Int = enclosing.cols

      def elem(r: Int, c: Int): F = enclosing(if (r >= ridx) r + 1 else r, c)
    }
  }

  def withoutCol(cidx: Int): Matrix[F] = {
    require(1 <= cidx && cidx <= cols, s"Matrix.withoutCol: column out of range: $cidx")

    val enclosing = this

    new Matrix {
      val rows: Int = enclosing.rows
      val cols: Int = enclosing.cols - 1

      def elem(r: Int, c: Int): F = enclosing(r, if (c >= cidx) c + 1 else c)
    }
  }

  def row(ridx: Int): Matrix[F] = block(ridx, 1, 1, cols)

  def col(cidx: Int): Matrix[F] = block(1, rows, cidx, 1)

  def operation(that: Matrix[F], name: String, op: (F, F) => F)(implicit t: ClassTag[F]): Matrix[F] = {
    require(rows == that.rows && cols == that.cols, s"$name: operand matrices must be of equal dimension")
    new ConcreteMatrix(rows, cols, (i, j) => op(this(i, j), that(i, j)))
  }

  def transpose(implicit t: ClassTag[F]) = new ConcreteMatrix(cols, rows, (i, j) => this(j, i))

  def scale(s: F)(implicit t: ClassTag[F]) = new ConcreteMatrix(rows, cols, (i, j) => this(i, j) * s)

  def add(that: Matrix[F])(implicit t: ClassTag[F]): Matrix[F] = operation(that, "Matrix.add", _ + _)

  def +(that: Matrix[F])(implicit t: ClassTag[F]): Matrix[F] = add(that)

  def sub(that: Matrix[F])(implicit t: ClassTag[F]): Matrix[F] = operation(that, "Matrix.sub", _ - _)

  def -(that: Matrix[F])(implicit t: ClassTag[F]): Matrix[F] = sub(that)

  def prod(that: Matrix[F]): F = {
    require(isVector && that.isVector, "Matrix.prod: operands must be vectors")
    require(length == that.length, "Matrix.prod: operands must be of equal length")
    this zip that map { case (a, b) => a * b } sum
  }

  def elemMul(that: Matrix[F])(implicit t: ClassTag[F]): Matrix[F] = operation(that, "Matrix.elemMul", _ * _)

  def mul(that: Matrix[F])(implicit t: ClassTag[F]): Matrix[F] = {
    require(cols == that.rows, "Matrix.mul: width of left operand must equal height of right operand")
    new ConcreteMatrix(rows, that.cols, (i, j) => this.row(i) prod that.col(j))
  }

  def *(that: Matrix[F])(implicit t: ClassTag[F]): Matrix[F] = mul(that)

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

class ConcreteMatrix[F](val rows: Int, val cols: Int, init: (Int, Int) => F)(implicit t: ClassTag[F],
                                                                             field: Fractional[F])
    extends Matrix[F] {

  private val data = ArraySeq.tabulate[F](rows, cols)((i: Int, j: Int) => init(i + 1, j + 1))

  def elem(row: Int, col: Int): F = data(row - 1)(col - 1)

}

object Matrix {

  def apply[F](data: Seq[Seq[F]])(implicit t: ClassTag[F], field: Fractional[F]): Matrix[F] = {
    require(data.nonEmpty, "Matrix cannot be empty")
    require(data forall (_.nonEmpty), "Matrix cannot have an empty row")
    require(data forall (_.length == data.head.length), "Matrix must have rows of same length")
    new ConcreteMatrix[F](data.length, data.head.length, (x: Int, y: Int) => data(x - 1)(y - 1))
  }

  def build[F](rows: Int, cols: Int, init: (Int, Int) => F)(implicit t: ClassTag[F], field: Fractional[F]) =
    new ConcreteMatrix[F](rows, cols, init)

  def diagonal[F](size: Int, value: F)(implicit t: ClassTag[F], field: Fractional[F]) =
    new ConcreteMatrix[F](size, size, (i, j) => if (i == j) value else field.zero)

  def identity[F](size: Int)(implicit t: ClassTag[F], field: Fractional[F]): Matrix[F] = diagonal(size, field.one)

  def row[F](elems: F*)(implicit t: ClassTag[F], field: Fractional[F]): Matrix[F] = {
    require(elems.nonEmpty, "Matrix.row: need at least one element")
    Matrix(List(elems))
  }

  def col[F](elems: F*)(implicit t: ClassTag[F], field: Fractional[F]): Matrix[F] = {
    require(elems.nonEmpty, "Matrix.col: need at least one element")
    new ConcreteMatrix(elems.length, 1, (r, _) => elems(r - 1))
  }

  def cath[F](mat: Matrix[F], mats: Matrix[F]*)(implicit t: ClassTag[F], field: Fractional[F]): Matrix[F] = {
    require(mats.forall(m => m.rows == mat.rows),
            "Matrix.cath: matrices being horizontally concatenated must have same height")

    val ms = new ArrayBuffer[(Matrix[F], Int)]
    var rcols = 0

    for (m <- mat +: mats) {
      for (i <- 1 to m.cols)
        ms append ((m, i))

      rcols += m.cols
    }

    new ConcreteMatrix(mat.rows, rcols, (i, j) => ms(j - 1)._1(i, ms(j - 1)._2))
  }

  def catv[F](mat: Matrix[F], mats: Matrix[F]*)(implicit t: ClassTag[F], field: Fractional[F]): Matrix[F] = {
    require(mats.forall(m => m.cols == mat.cols),
            "Matrix.catv: matrices being vertically concatenated must have same width")

    val ms = new ArrayBuffer[(Matrix[F], Int)]
    var rrows = 0

    for (m <- mat +: mats) {
      for (i <- 1 to m.rows)
        ms append ((m, i))

      rrows += m.rows
    }

    new ConcreteMatrix(rrows, mat.cols, (i, j) => ms(i - 1)._1(ms(i - 1)._2, j))
  }

}
