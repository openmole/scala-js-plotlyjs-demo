package plotlyjs.demo.utils

import plotlyjs.demo.utils.Vectors._

object Matrices {

  type Matrix = Seq[Vector]

  def matrix(rows: Int, columns: Int, f: (Int, Int) => Double): Matrix = (0 until rows).map(i => (0 until columns).map(j => f(i, j)))

  implicit class ImplicitMatrix(thisMatrix: Matrix) {

    def mapWithIndex(f: (Int, Int, Double) => Double): Matrix = thisMatrix.zipWithIndex map { case (v, i) => v.zipWithIndex map { case (c, j) => f(i, j, c) } }

    def +(otherMatrix: Matrix): Matrix = thisMatrix.zip(otherMatrix) map { case (v1, v2) => v1 + v2 }
    def -(otherMatrix: Matrix): Matrix = thisMatrix.zip(otherMatrix) map { case (v1, v2) => v1 - v2 }
    def mul(otherMatrix: Matrix): Matrix = thisMatrix.zip(otherMatrix) map { case (v1, v2) => v1 mul v2 }

    def *(otherMatrix: Matrix): Matrix = {
      thisMatrix.map(v1 =>
        otherMatrix.transpose.map(v2 =>
          v1 dot v2
        )
      )
    }

    def mulRow(vector: Vector): Vector = (thisMatrix * vector.toRowMatrix).head
    def mulColumn(vector: Vector): Vector = (thisMatrix * vector.toColumnMatrix).transpose.head

  }

}
