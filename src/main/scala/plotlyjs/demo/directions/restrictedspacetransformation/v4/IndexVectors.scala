package plotlyjs.demo.directions.restrictedspacetransformation.v4

import plotlyjs.demo.utils.Vectors.Vector

import scala.language.implicitConversions

object IndexVectors {

  type IndexVector = Seq[Int]

  implicit class ImplicitIndexVector(indexVector: IndexVector) {

    def vector: Vector = indexVector.map(_.toDouble)

  }

  implicit def implicitToVector(i: IndexVector): Vector = i.map(_.toDouble)

  implicit def implicitToIndex(v: Vector): IndexVector = v.map(math.rint(_).toInt)

}
