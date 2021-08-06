package plotlyjs.demo.utils.vector

import plotlyjs.demo.utils.vector.Vectors._

import scala.language.implicitConversions
import scala.math._

object IntVectors {

  type IntVector = Seq[Int]

  implicit class ImplicitIntVector(intVector: IntVector) {
    def vector: Vector = intVector.map(_.toDouble)

    def intVectorToString: String = "(" + intVector.mkString(", ") + ")"
  }

  implicit def toVector(i: IntVector): Vector = i.map(_.toDouble)

  implicit def toIntVector(v: Vector): IntVector = v.map(rint(_).toInt)



  def positiveNCube(dimension: Int, p: Int): Iterable[IntVector] = {
    new Iterable[IntVector] {
      override def iterator: Iterator[IntVector] = new Iterator[IntVector] {

        private val pointGenerator = Array.fill[Int](dimension)(p - 1)
        private var count = 0

        override def hasNext: Boolean = count < pow(p, dimension)

        override def next(): IntVector = {
          pointGenerator(0) += 1
          for (i <- 0 until dimension if pointGenerator(i) == p) {
            pointGenerator(i) = 0
            if (i + 1 < dimension) pointGenerator(i + 1) += 1
          }
          count = count + 1
          pointGenerator.toSeq
        }

      }
    }.view
  }

  def centeredNCube(dimension: Int, radius: Int): Iterable[IntVector] = {
    positiveNCube(dimension, 2 * radius + 1).map(toVector).map(_.sub(radius.toDouble))
  }

}
