package plotlyjs.demo.homemade.utils

import plotlyjs.demo.homemade.utils.Vectors._

import scala.language.implicitConversions
import scala.math._

object IntVectors {

  type IntVector = Seq[Int]

  implicit class ImplicitIntVector(intVector: IntVector) {
    def vector: Vector = intVector.map(_.toDouble)

    def intVectorToString: String = "(" + intVector.mkString(", ") + ")"
  }

  implicit def implicitToVector(i: IntVector): Vector = i.map(_.toDouble)

  implicit def toIntVector(v: Vector): IntVector = v.map(rint(_).toInt)



  def vectorIndices(sizes: Seq[Int]): Iterable[IntVector] = {
    if(sizes.isEmpty) {
      new Iterable[IntVector] {
        override def iterator: Iterator[IntVector] = new Iterator[IntVector] {
          override def hasNext: Boolean = false
          override def next(): IntVector = ???
        }
      }
    } else {
      new Iterable[IntVector] {
        override def iterator: Iterator[IntVector] = new Iterator[IntVector] {

          private val dimension = sizes.size
          private val totalCount = sizes.product

          private val pointGenerator = sizes.map(_ - 1).toArray//Array.fill[Int](dimension)(_size - 1)
          private var count = 0

          override def hasNext: Boolean = count < totalCount

          override def next(): IntVector = {
            pointGenerator(0) += 1
            for(i <- 0 until dimension if pointGenerator(i) == sizes(i)) {
              pointGenerator(i) = 0
              if(i + 1 < dimension) pointGenerator(i + 1) += 1
            }
            count = count + 1
            pointGenerator
          }

        }
      }.view
    }
  }

  def positiveNCube(dimension: Int, p: Int): Iterable[IntVector] = {
    if(dimension == 0) {
      new Iterable[IntVector] {
        override def iterator: Iterator[IntVector] = new Iterator[IntVector] {
          override def hasNext: Boolean = false
          override def next(): IntVector = ???
        }
      }
    } else {
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
  }

  def centeredNCube(dimension: Int, radius: Int): Iterable[IntVector] = {
    positiveNCube(dimension, 2 * radius + 1).map(_.vector.sub(radius.toDouble))
  }

}
