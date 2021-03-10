package xyz.hyperreal.matrix

import scala.collection.immutable.{AbstractSeq, ArraySeq, IndexedSeq}
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import math.Fractional.Implicits._
import math.Ordering.Implicits._
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
      case that: Matrix[F] => (that canEqual this) && rows == that.rows && cols == that.cols && (elements forall { case (i, j, v) => v == that.elem(i, j) })
      case _               => false
    }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Matrix[F]]

  override def hashCode: Int = map(_.hashCode) reduce (_ ^ _)

  protected def elem(r: Int, c: Int): F

  protected def boundsCheck(r: Int, c: Int, name: String): Unit = {
    require(1 <= r && r <= rows, s"Matrix.$name: row out of range: $r")
    require(1 <= c && c <= cols, s"Matrix.$name: column out of range: $c")
  }

  lazy val dim: (Int, Int) = (rows, cols)

  lazy val length: Int = rows * cols

  lazy val isRow: Boolean = rows == 1

  lazy val isColumn: Boolean = cols == 1

  lazy val isVector: Boolean = isRow || isColumn

  def elements: Iterator[(Int, Int, F)] =
    0 until length map { idx =>
      val (r, c) = (idx % rows + 1, idx / rows + 1)

      (r, c, elem(r, c))
    } iterator

  lazy val isZero: Boolean = this forall (_ == field.zero)

  lazy val isDiagonal: Boolean = elements forall { case (i, j, v) => i == j || v == field.zero }

  lazy val isSquare: Boolean = rows == cols

  lazy val isSymmetric: Boolean = isSquare && this == transpose

  lazy val isSkewSymmetric: Boolean = isSquare && elements.forall { case (i, j, v) => v == -elem(j, i) }

  lazy val isOrthogonal: Boolean = isSquare && transpose == inverse

  lazy val upperTriangular: Boolean = elements forall { case (i, j, v) => i <= j || v == field.zero }

  lazy val lowerTriangular: Boolean = elements forall { case (i, j, v) => i >= j || v == field.zero }

  lazy val diagonal: Iterator[F] = (1 to (rows min cols)).iterator map (i => elem(i, i))

  def submatrix(i: Int, j: Int): Matrix[F] = {
    boundsCheck(i, j, s"submatrix")
    removeRow(i).removeCol(j)
  }

  def minor(i: Int, j: Int): F = {
    boundsCheck(i, j, s"minor")
    submatrix(i, j).det
  }

  def cofactor(i: Int, j: Int): F = {
    boundsCheck(i, j, s"cofactor")
    if ((i + j) % 2 == 1) -minor(i, j) else minor(i, j)
  }

  lazy val trace: F = {
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

  lazy val inverse: Matrix[F] = {
    require(isSquare, "Matrix.inv: must be a square matrix")

    rows match {
      case 1 => Matrix.row(field.one / elem(1, 1))
      case _ => adj / det
    }
  }

  lazy val adj: Matrix[F] = {
    require(isSquare, "Matrix.adj: must be a square matrix")

    rows match {
      case 1 if elem(1, 1) == field.zero => this
      case 1                             => Matrix.row(field.one)
      case 2                             => Matrix.rows(2, elem(2, 2), -elem(1, 2), -elem(2, 1), elem(1, 1))
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

  lazy val vec: Matrix[F] = Matrix.fromIndexedSeq(1, this)

  def build(init: (Int, Int) => F) = new ConcreteMatrix(rows, cols, init)

  def concrete: Matrix[F] = build(elem)

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

  def replace(v: Matrix[F], idx: Int): Matrix[F] = {
    require(v.isVector, "can only replace a row or column vector")

    if (v.isColumn) {
      require(1 <= idx && idx <= cols, s"column index out of range: $idx")

      idx match {
        case 1      => Matrix.cath(v, removeCol(1))
        case `cols` => Matrix.cath(removeCol(cols), v)
        case _      => Matrix.cath(block(1, rows, 1, idx - 1), v, block(1, rows, idx + 1, cols - idx))
      }
    } else {
      require(1 <= idx && idx <= rows, s"row index out of range: $idx")

      idx match {
        case 1      => Matrix.catv(v, removeRow(1))
        case `rows` => Matrix.catv(removeRow(cols), v)
        case _      => Matrix.catv(block(1, idx - 1, 1, cols), v, block(idx + 1, rows - idx, 1, cols))
      }
    }
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

  def removeRow(ridx: Int): Matrix[F] = {
    require(1 <= ridx && ridx <= rows, s"Matrix.removeRow: row out of range: $ridx")

    val outer = this

    new Matrix {
      val rows: Int = outer.rows - 1
      val cols: Int = outer.cols

      def elem(r: Int, c: Int): F = outer.elem(if (r >= ridx) r + 1 else r, c)
    }
  }

  def removeCol(cidx: Int): Matrix[F] = {
    require(1 <= cidx && cidx <= cols, s"Matrix.removeCol: column out of range: $cidx")

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

  def map(f: F => F): Matrix[F] = build((i, j) => f(elem(i, j)))

  def *(s: F): Matrix[F] = map(_ * s)

  def /(s: F): Matrix[F] = map(_ / s)

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

  lazy val rowEchelonForm: ConcreteMatrix[F] = Matrix.fromArray(echelon(false))

  lazy val reducedRowEchelonForm: ConcreteMatrix[F] = Matrix.fromArray(echelon(true))

  lazy val rank: Int = nonZeroDiagonals(echelon(false))

  protected def nonZeroDiagonals(a: Array[Array[F]]): Int =
    (0 until (rows min cols)).iterator map (i => a(i)(i)) count (_ == field.one)

  lazy val solution: ConcreteMatrix[F] = {
    val a = echelon(true)
    val r = nonZeroDiagonals(a)

    Matrix.fromIndexedSeq(1, 0 until r map (a(_)(cols - 1)))
  }

  def array: Array[Array[F]] = Array.tabulate[F](rows, cols)((i: Int, j: Int) => elem(i + 1, j + 1))

  lazy val LU: (ConcreteMatrix[F], ConcreteMatrix[F]) = {
    val a = array
    val l = Array.fill[F](rows, cols)(field.zero)
    val u = Array.fill[F](rows, cols)(field.zero)

    for (k <- 0 until rows) {
      u(k)(k) = a(k)(k)

      for (i <- k + 1 until rows) {
        l(i)(k) = a(i)(k) / u(k)(k)
        u(k)(i) = a(k)(i)
      }

      for (i <- k + 1 until rows; j <- k + 1 until rows)
        a(i)(j) -= l(i)(k) * u(k)(j)
    }

    for (i <- 0 until rows)
      l(i)(i) = field.one

    (Matrix.fromArray(l), Matrix.fromArray(u))
  }

  lazy val LUP: (ConcreteMatrix[F], ConcreteMatrix[F], ConcreteMatrix[F]) = {
    require(isSquare, "LUP: must be a square matrix")

    val a = array
    val perm = Array.tabulate(rows)(identity)

    for (k <- 0 until rows) {
      var p = field.zero
      var kp = 0

      for (i <- k until rows)
        if (field.abs(a(i)(k)) > p) {
          p = field.abs(a(i)(k))
          kp = i
        }

      if (p == field.zero)
        sys.error("LUP: singular matrix")

      val t = perm(k)

      perm(k) = perm(kp)
      perm(kp) = t
      swap(k, kp, a)

      for (i <- k + 1 until rows) {
        a(i)(k) /= a(k)(k)

        for (j <- k + 1 until rows)
          a(i)(j) -= a(i)(k) * a(k)(j)
      }
    }

    val l = build((i, j) => if (i > j) a(i - 1)(j - 1) else if (i == j) field.one else field.zero)
    val u = build((i, j) => if (i <= j) a(i - 1)(j - 1) else field.zero)
    val p = Array.fill[F](rows, cols)(field.zero)

    for (i <- perm indices)
      p(i)(perm(i)) = field.one

    (l, u, Matrix.fromArray(p))
  }

  protected def swap(r1: Int, r2: Int, a: Array[Array[F]]): Unit =
    for (i <- 0 until cols) {
      val t = a(r1)(i)

      a(r1)(i) = a(r2)(i)
      a(r2)(i) = t
    }

  protected def echelon(reduced: Boolean): Array[Array[F]] = {
    val a = array

    def one(r: Int): Unit = {
      val pivot = a(r)(r)

      if (pivot != field.one) {
        println(s"r${r + 1}/$pivot")

        for (i <- r until cols) {
          a(r)(i) /= pivot
        }
      }
    }

    def zero(rs: Int, rt: Int): Unit = {
      val s = a(rt)(rs)

      if (s != field.zero) {
        a(rt)(rs) = field.zero
//        print(s"r${rt + 1} -= $s * r${rs + 1} --> 0 ")

        for (i <- rs + 1 until cols) {
          a(rt)(i) -= s * a(rs)(i)
//          print(s"${array(rt)(i)} ")
        }

//        println
      }
    }

    for (i <- 0 until (rows min cols)) {
      if (a(i)(i) == field.zero) {
        (i until rows filter (_ != i)).find(r => a(r)(i) != field.zero) match {
          case Some(row) =>
//            println(s"r${row + 1} <-> r${i + 1}")
            swap(row, i, a)
          case None =>
        }
      }

      if (a(i)(i) != field.zero) {
        one(i)

        for (j <- (0 until (if (reduced) i else 0)) ++ (i + 1 until rows))
          zero(i, j)
      }
    }

    a
  }

  def table: String =
    new TextTable(matrix = true) {
      for (i <- 1 to rows)
        rowSeq(Matrix.this.row(i))

      1 to cols foreach rightAlignment
    }.toString

  override def toString: String = {
    val buf = new StringBuilder("Matrix(")

    for (i <- 1 to rows) {
      buf ++= row(i).mkString("[", ", ", "]")

      if (i < rows)
        buf ++= ", "
    }

    buf += ')'
    buf.toString
  }

}

class ConcreteMatrix[F](val rows: Int, val cols: Int, init: (Int, Int) => F)(implicit t: ClassTag[F], field: Fractional[F]) extends Matrix[F] {

  private val data = ArraySeq.tabulate[F](rows, cols)((i: Int, j: Int) => init(i + 1, j + 1))

  def elem(row: Int, col: Int): F = data(row - 1)(col - 1)

}

object Matrix {

  def apply[F](data: Seq[F]*)(implicit t: ClassTag[F], field: Fractional[F]): Matrix[F] = {
    require(data.nonEmpty, "Matrix cannot be empty")
    require(data forall (_.nonEmpty), "Matrix cannot have an empty row")

    val width = data.head.length

    require(data forall (_.length == width), "Matrix must have rows of same length")
    Matrix.fromIndexedSeq(width, ArrayBuffer.concat[F](data: _*))
  }

  def rows[F](width: Int, elems: F*)(implicit t: ClassTag[F], field: Fractional[F]): ConcreteMatrix[F] = {
    require(elems.nonEmpty, "matrix cannot be empty")
    require(elems.length % width == 0, "wrong number of elements")
    build(elems.length / width, width, (i, j) => elems((i - 1) * width + j - 1))
  }

  def fromIndexedSeq[F](width: Int, elems: collection.IndexedSeq[F])(implicit t: ClassTag[F], field: Fractional[F]): ConcreteMatrix[F] = {
    require(elems.nonEmpty, "matrix cannot be empty")
    require(elems.length % width == 0, "wrong number of elements")
    build(elems.length / width, width, (i, j) => elems((i - 1) * width + j - 1))
  }

  def fromArray[F](data: Array[Array[F]])(implicit t: ClassTag[F], field: Fractional[F]): ConcreteMatrix[F] = {
    require(data.nonEmpty, "Matrix cannot be empty")
    require(data forall (_.nonEmpty), "Matrix cannot have an empty row")
    require(data forall (_.length == data.head.length), "Matrix must have rows of same length")
    build(data.length, data.head.length, (x: Int, y: Int) => data(x - 1)(y - 1))
  }

  def build[F](rows: Int, cols: Int, init: (Int, Int) => F)(implicit t: ClassTag[F], field: Fractional[F]) =
    new ConcreteMatrix[F](rows, cols, init)

  def diagonal[F](size: Int, value: F)(implicit t: ClassTag[F], field: Fractional[F]): ConcreteMatrix[F] =
    build[F](size, size, (i, j) => if (i == j) value else field.zero)

  def zero[F](size: Int)(implicit t: ClassTag[F], field: Fractional[F]): ConcreteMatrix[F] =
    build[F](size, size, (_, _) => field.zero)

  def identity[F](size: Int)(implicit t: ClassTag[F], field: Fractional[F]): Matrix[F] = diagonal(size, field.one)

  def row[F](elems: F*)(implicit t: ClassTag[F], field: Fractional[F]): Matrix[F] = {
    require(elems.nonEmpty, "Matrix.row: need at least one element")
    build(1, elems.length, (_, c) => elems(c - 1))
  }

  def col[F](elems: F*)(implicit t: ClassTag[F], field: Fractional[F]): Matrix[F] = {
    require(elems.nonEmpty, "Matrix.col: need at least one element")
    build(elems.length, 1, (r, _) => elems(r - 1))
  }

  def cath[F](mat: Matrix[F], mats: Matrix[F]*)(implicit t: ClassTag[F], field: Fractional[F]): Matrix[F] = {
    require(mats.forall(m => m.rows == mat.rows), "Matrix.cath: matrices being horizontally concatenated must have same height")

    val ms = new ArrayBuffer[(Matrix[F], Int)]
    var rcols = 0

    for (m <- mat +: mats) {
      for (i <- 1 to m.cols)
        ms append ((m, i))

      rcols += m.cols
    }

    build(mat.rows, rcols, (i, j) => ms(j - 1)._1(i, ms(j - 1)._2))
  }

  def catv[F](mat: Matrix[F], mats: Matrix[F]*)(implicit t: ClassTag[F], field: Fractional[F]): Matrix[F] = {
    require(mats.forall(m => m.cols == mat.cols), "Matrix.catv: matrices being vertically concatenated must have same width")

    val ms = new ArrayBuffer[(Matrix[F], Int)]
    var rrows = 0

    for (m <- mat +: mats) {
      for (i <- 1 to m.rows)
        ms append ((m, i))

      rrows += m.rows
    }

    build(rrows, mat.cols, (i, j) => ms(i - 1)._1(ms(i - 1)._2, j))
  }

}
