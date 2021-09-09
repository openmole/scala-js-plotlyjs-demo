package plotlyjs.demo.labo.directions.angularadjustment

import AngularAdjustment.Geometry._
import AngularAdjustment.Splitter._
import plotlyjs.demo.utils.vector.Vectors._

object AngularAdjustment {

  object Splitter {

    type Splitter = Vector => Vector

    implicit class ImplicitSplitter(splitter: Splitter) {
      def component: Splitter = splitter

      def remainder(vector: Vector): Vector = vector - component(vector)

      def split(vector: Vector): Splitting = {
        val componentOfVector = component(vector)
        (componentOfVector, vector - componentOfVector)
      }
    }

    type Splitting = (Vector, Vector)

    implicit class ImplicitSplitting(splitting: Splitting) {
      val component: Vector = splitting._1
      val remainder: Vector = splitting._2
      lazy val fusion: Vector = component + remainder
    }

    val MaxMagnitudeComponent: Splitter = (v: Vector) => { //Max dot product on basis vectors and their negatives versions.
      val maxMagnitudeIndex = v.map(math.abs).zipWithIndex.maxBy(_._1)._2
      v.zipWithIndex.map { case (c, i) => if (i == maxMagnitudeIndex) c else 0 }
    }

  }

  //TODO : component corresponds to the higher scalar product on "simple" reference directions
  // reference directions are the dual of the n-dimensional regular polyhedron
  // with the corresponding Splitters for its cells and recursively
  // making the Splitter list of DirectionsSegmentation

  object Geometry {

    trait Geometry { //TODO a splitter list, one for each space (n, n-1, n-2, ..., 2, 1 ?) – for the future point generation algorithm ?
      def radialSplitter: Splitter //future n to (n-1)-cell projector

      def borderNormalSplitter: Splitter //future n-1 to (n-2)-cell projector

      def radialSplit(vector: Vector): Splitting = radialSplitter.split(vector)

      def borderNormalSplit(vector: Vector): Splitting = borderNormalSplitter.split(vector)

      def space(dimension: Int): Double
    }

    val cubic: Geometry = new Geometry {
      override def radialSplitter: Splitter = Splitter.MaxMagnitudeComponent

      override def borderNormalSplitter: Splitter = Splitter.MaxMagnitudeComponent

      override def space(dimension: Int): Double = 4 * 2 * dimension //TODO Geometry as a GeometryFactory to set dimension at the beginning ?
    }

  }

  def cellRadialAdjustment(geometry: Geometry, vector: Vector): Vector = {
    val (componentToKeep, remainderToAdjust) = geometry.radialSplit(vector)
    val sphericalRadius = componentToKeep.norm
    val sphericalRadialDirection = componentToKeep.normalize

    val (borderNormalComponent, _) = geometry.borderNormalSplit(remainderToAdjust)
    val centerToBorderProportion = borderNormalComponent.norm / sphericalRadius

    val touchingBorderRemainder = (1 / centerToBorderProportion) * remainderToAdjust
    val touchingBorder = componentToKeep + touchingBorderRemainder
    val maxAngle = touchingBorder ^ sphericalRadialDirection

    val newVectorAngle = centerToBorderProportion * maxAngle

    val newRemainderLength = sphericalRadius * math.tan(newVectorAngle)
    val adjustedRemainder = remainderToAdjust toNorm newRemainderLength
    val adjustedVector = componentToKeep + adjustedRemainder

    if (adjustedVector.count(_.isNaN) == 0) adjustedVector else vector
  }

  def nSphereSurface(n: Int, r: Double): Double = {
    import scala.math.{Pi, pow}
    if (n % 2 == 0) {
      pow(2, n / 2 + 1) * pow(Pi, n / 2) * pow(r, n) / (1 to n - 1 by 2).map(_.toDouble).product
    } else {
      pow(Pi, (n + 1) / 2) * pow(r, n) / (1.0 / 2.0 * (1 to (n - 1) / 2).map(_.toDouble).product)
    }
  }

  def spacialAdjustment(geometry: Geometry, dimension: Int): Double = {
    math.pow(geometry.space(dimension) / nSphereSurface(dimension - 1, 1), 1.0 / dimension)
  }

  def spacialAdjustedNormalization(geometry: Geometry, vector: Vector): Vector = {
    val dimension = vector.length
    val radius = {
      val (radialComponent, _) = geometry.radialSplit(vector)
      radialComponent.norm
    }
    vector toNorm radius * spacialAdjustment(geometry, dimension)
  }

}
