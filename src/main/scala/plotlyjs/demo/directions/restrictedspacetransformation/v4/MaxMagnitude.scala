package plotlyjs.demo.directions.restrictedspacetransformation.v4

import plotlyjs.demo.utils.Vectors._

import scala.math._

case class MaxMagnitude(vector: Vector) {

  lazy val index: Int = vector.indices.maxBy(i => abs(vector(i)))
  lazy val coordinate: Double = vector(index)
  lazy val signum: Double = math.signum(coordinate)
  lazy val value: Double = abs(coordinate)
  lazy val fullSpaceComponent: Vector = vector.zipWithIndex.map { case (c, i) => if (i == index) c else 0 }
  lazy val fullSpaceRemainder: Vector = vector - fullSpaceComponent
  lazy val remainderSpaceRemainder: Vector = vector.zipWithIndex.filterNot(_._2 == index).map(_._1)

  def reconnect(newRemainderSpaceRemainder: Vector): Vector = {
    newRemainderSpaceRemainder.insert(index, coordinate)
  }

}
