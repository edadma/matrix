package xyz.hyperreal.matrix

import
abstract class Matrix extends ((Int, Int) => Number) {

  val rows: Int
  val cols: Int

  def +(that: Matrix): Matrix =
    new Matrix {
      val rows: Int = Matrix.this.rows
      val cols: Int = Matrix.this.cols

      require( rows == that.rows && cols == that.cols, "Matrix: only matrices of equal dimension can be added")

      def apply(row: Int, col: Int): Number = this(row, col)
    }
}

class ConcreteMatrix(val rows: Int, val cols: Int, init: (Int, Int) => Number) extends Matrix {

  private val data = Array.ofDim[Number](rows, cols)

  for (i <- 1 to rows; j <- 1 to cols)
    data(i - 1)(j - 1) = init(i, j)

  def apply(row: Int, col: Int): Number = {
    require(0 < row && row <= rows, s"Matrix: row out of range: $row")
    require(0 < col && col <= cols, s"Matrix: row out of range: $col")
    data(row - 1)(col - 1)
  }

}

object Matrix {

  def apply(contents: Seq[Seq[Number]]): Unit = {
    require(contents.nonEmpty, "Matrix cannot be empty")
    require(contents forall (_.nonEmpty), "Matrix cannot have an empty row")
    require(contents forall (_.length == contents.head.length), "Matrix must have rows of same length")

  }

}
