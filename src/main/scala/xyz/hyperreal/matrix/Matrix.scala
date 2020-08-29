package xyz.hyperreal.matrix

import scala.collection.immutable.{IndexedSeq, AbstractSeq, ArraySeq}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import math.Fractional.Implicits._

import xyz.hyperreal.table.TextTable

abstract class Matrix[F](implicit classTag: ClassTag[F], field: Fractional[F])
    extends AbstractSeq[F]
    with IndexedSeq[F]
    with ((Int, Int) => F)
    with Equals {

  val rows: Int
  val cols: Int

  override def equals(other: Any): Boolean =
    other match {
      case that: Matrix[F] => (that canEqual this) && (elements forall { case (i, j, v) => v == that.elem(i, j) })
      case _               => false
    }

  override def canEqual(other: Any): Boolean =
    other.isInstanceOf[Matrix[F]]

  override def hashCode: Int = map(_.hashCode) reduce (_ ^ _)

  def elem(r: Int, c: Int): F

  protected def boundsCheck(r: Int, c: Int, name: String): Unit = {
    require(1 <= r && r <= rows, s"Matrix.$name: row out of range: $r")
    require(1 <= c && c <= cols, s"Matrix.$name: column out of range: $c")
  }

  lazy val dim: (Int, Int) = (rows, cols)

  lazy val length: Int = rows * cols

  lazy val isRow: Boolean = rows == 1

  lazy val isColumn: Boolean = cols == 1

  lazy val isVector: Boolean = isRow || isColumn

  lazy val elements: Seq[(Int, Int, F)] = for (i <- 1 to cols; j <- 1 to rows) yield (i, j, elem(i, j))

  lazy val isZero: Boolean = this forall (_ == field.zero)

  lazy val isDiagonal: Boolean = elements forall { case (i, j, v) => i == j || v == field.zero }

  lazy val isSquare: Boolean = rows == cols

  lazy val isSymmetric: Boolean = isSquare && this == transpose

  lazy val isSkewSymmetric: Boolean = isSquare && elements.forall { case (i, j, v) => v == -elem(j, i) }

  lazy val isOrthogonal: Boolean = isSquare && transpose == inv

  def minor(i: Int, j: Int): F = {
    boundsCheck(i, j, s"minor")
    withoutRow(i).withoutCol(j).det
  }

  def cofactor(i: Int, j: Int): F = {
    boundsCheck(i, j, s"cofactor")
    if ((i + j) % 2 == 1) -minor(i, j) else minor(i, j)
  }

  lazy val tr: F = {
    require(isSquare, "Matrix.tr: must be a square matrix")

    1 to rows map (i => elem(i, i)) sum
  }

  lazy val det: F = {
    require(isSquare, "Matrix.det: must be a square matrix")

    rows match {
      case 1 => elem(1, 1)
      case 2 => elem(1, 1) * elem(2, 2) - elem(1, 2) * elem(2, 1)
      case _ => 1 to rows map (i => elem(i, 1) * cofactor(i, 1)) sum
    }
  }

  lazy val inv: Matrix[F] = {
    require(isSquare, "Matrix.inv: must be a square matrix")

    rows match {
      case 1 => Matrix(List(List(field.one / elem(1, 1))))
      case _ => adj / det
    }

  }

  lazy val adj: Matrix[F] = {
    require(isSquare, "Matrix.adj: must be a square matrix")

    rows match {
      case 1 if elem(1, 1) == field.zero => this
      case 1                             => Matrix(Seq(Seq(field.one)))
      case 2                             => Matrix(Seq(Seq(elem(2, 2), -elem(1, 2)), Seq(-elem(2, 1), elem(1, 1))))
      case _                             => build((i, j) => cofactor(j, i))
    }
  }

  lazy val transpose: Matrix[F] = {
    val outer = this

    new Matrix {
      val rows: Int = outer.rows
      val cols: Int = outer.cols

      def elem(i: Int, j: Int): F = outer.elem(j, i)
    }
  }

  def apply(r: Int, c: Int): F = {
    boundsCheck(r, c, "apply")
    elem(r, c)
  }

  def apply(idx: Int): F = {
    require(0 <= idx && idx < length, s"Matrix (as Seq).apply: index out of range: $idx")
    elem(idx % rows + 1, idx / rows + 1)
  }

  def vec: Matrix[F] = Matrix(map(Seq(_)))

  def build(init: (Int, Int) => F) = new ConcreteMatrix(rows, cols, init)

  def concrete: ConcreteMatrix[F] = build(elem)

  def prepend(v: Matrix[F]): Matrix[F] = {
    require(v.isVector, "can only prepend a row or column vector")

    if (v.isColumn)
      Matrix.cath(v, this)
    else
      Matrix.catv(v, this)
  }

  def append(v: Matrix[F]): Matrix[F] = {
    require(v.isVector, "can only append a row or column vector")

    if (v.isColumn)
      Matrix.cath(this, v)
    else
      Matrix.catv(this, v)
  }

  def block(ridx: Int, height: Int, cidx: Int, width: Int): Matrix[F] = {
    require(1 <= ridx && ridx <= rows, s"Matrix.view: row out of range: $ridx")
    require(1 <= cidx && cidx <= cols, s"Matrix.view: col out of range: $cidx")
    require(1 <= height && height <= rows, s"Matrix.view: height out of range: $height")
    require(1 <= width && width <= cols, s"Matrix.view: width out of range: $width")

    val outer = this

    new Matrix {
      val rows: Int = height
      val cols: Int = width

      def elem(r: Int, c: Int): F = outer.elem(r + ridx - 1, c + cidx - 1)
    }
  }

  def withoutRow(ridx: Int): Matrix[F] = {
    require(1 <= ridx && ridx <= rows, s"Matrix.withoutRow: row out of range: $ridx")

    val outer = this

    new Matrix {
      val rows: Int = outer.rows - 1
      val cols: Int = outer.cols

      def elem(r: Int, c: Int): F = outer.elem(if (r >= ridx) r + 1 else r, c)
    }
  }

  def withoutCol(cidx: Int): Matrix[F] = {
    require(1 <= cidx && cidx <= cols, s"Matrix.withoutCol: column out of range: $cidx")

    val outer = this

    new Matrix {
      val rows: Int = outer.rows
      val cols: Int = outer.cols - 1

      def elem(r: Int, c: Int): F = outer.elem(r, if (c >= cidx) c + 1 else c)
    }
  }

  def row(ridx: Int): Matrix[F] = block(ridx, 1, 1, cols)

  def col(cidx: Int): Matrix[F] = block(1, rows, cidx, 1)

  def operation(that: Matrix[F], name: String, op: (F, F) => F): Matrix[F] = {
    require(rows == that.rows && cols == that.cols, s"$name: operand matrices must be of equal dimension")
    build((i, j) => op(elem(i, j), that(i, j)))
  }

  def *(s: F): ConcreteMatrix[F] = build((i, j) => elem(i, j) * s)

  def /(s: F): ConcreteMatrix[F] = build((i, j) => elem(i, j) / s)

  def add(that: Matrix[F]): Matrix[F] = operation(that, "Matrix.add", _ + _)

  def +(that: Matrix[F]): Matrix[F] = add(that)

  def sub(that: Matrix[F]): Matrix[F] = operation(that, "Matrix.sub", _ - _)

  def -(that: Matrix[F]): Matrix[F] = sub(that)

  def prod(that: Matrix[F]): F = {
    require(isVector && that.isVector, "Matrix.prod: operands must be vectors")
    require(length == that.length, "Matrix.prod: operands must be of equal length")
    this zip that map { case (a, b) => a * b } sum
  }

  def elemMul(that: Matrix[F]): Matrix[F] = operation(that, "Matrix.elemMul", _ * _)

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
