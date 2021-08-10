package plotlyjs.demo.utils.vector

import plotlyjs.demo.utils.graph.directed.Graph
import plotlyjs.demo.utils.graph.directed.Graph.ImplicitTail
import plotlyjs.demo.utils.vector.Vectors._

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
    positiveNCube(dimension, 2 * radius + 1).map(_.vector.sub(radius.toDouble))
  }

  def nGrid(dimension: Int, p: Int): Graph[IntVector] = {
    if(dimension == 0) Graph(Seq()) else {
      val graph = nGrid(dimension - 1, p)
      val duplicates = (0 until p).map(i => graph.mapVertices(_ ++ Seq(i))).reduce(_ ++ _)
      duplicates ++ Graph.from(graph.vertices.flatMap(v => {
        (1 until p).map(i => {
          (v ++ Seq(i - 1)) --> (v ++ Seq(i))
        })
      }))
    }
  }

}
