package plotlyjs.demo.directions.restrictedspacetransformation.v4

import plotlyjs.demo.utils.Vectors._

import scala.language.implicitConversions

object IndexVectors {

  type IndexVector = Seq[Int]

  implicit class ImplicitIndexVector(indexVector: IndexVector) {
    def vector: Vector = indexVector.map(_.toDouble)
    def indexVectorToString: String = "(" + indexVector.mkString(", ") + ")"
  }

  implicit def toVector(i: IndexVector): Vector = i.map(_.toDouble)
  implicit def toIndexVector(v: Vector): IndexVector = v.map(math.rint(_).toInt)



  def positiveNCube(dimension: Int, p: Int): Iterable[IndexVector] = {
    new Iterable[IndexVector] {
      override def iterator: Iterator[IndexVector] = new Iterator[IndexVector] {
        private val pointGenerator = Array.fill[Int](dimension)(p-1)
        private var count = 0
        override def hasNext: Boolean = count < math.pow(p, dimension)
        override def next(): IndexVector = {
          pointGenerator(0) += 1
          for(i <- 0 until dimension if pointGenerator(i) == p) {
            pointGenerator(i) = 0
            if(i + 1 < dimension) pointGenerator(i + 1) += 1
          }
          count = count + 1
          pointGenerator.toSeq
        }
      }
    }.view
  }

  def centeredNCube(dimension: Int, radius: Int): Iterable[IndexVector] = {
    positiveNCube(dimension, 2 * radius + 1).map(toVector).map(_.sub(radius.toDouble))
  }

}
