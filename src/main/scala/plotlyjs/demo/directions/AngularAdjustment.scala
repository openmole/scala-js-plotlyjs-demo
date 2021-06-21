package plotlyjs.demo.directions

import plotlyjs.demo.utils.Vectors._
import plotlyjs.demo.directions.AngularAdjustment.Geometry.Geometry

object AngularAdjustment {

  object Geometry {

    trait Splitter {
      def component(vector: Seq[Double]): Seq[Double] //TODO : component corresponds to the higher scalar product on "simple" reference directions
                                                      // reference directions are the dual of the n-dimensional regular polyhedron
                                                      // with the corresponding Splitters for its cells and recursively
                                                      // making the Splitter list of DirectionsSegmentation
      def split(vector: Seq[Double]): (Seq[Double], Seq[Double]) = {
        val componentOfVector = component(vector)
        (componentOfVector, vector - componentOfVector)
      }
    }

    trait Geometry { //TODO a splitter list, one for each space (n, n-1, n-2, ..., 2, 1 ?) – for the future point generation algorithm ?
      val radialSplitter: Splitter //future n to (n-1)-cell projector
      val borderNormalSplitter: Splitter //future n-1 to (n-2)-cell projector
      def radialSplit(vector: Seq[Double]): (Seq[Double], Seq[Double]) = radialSplitter.split(vector)
      def borderNormalSplit(vector: Seq[Double]): (Seq[Double], Seq[Double]) = borderNormalSplitter.split(vector)

      def space(dimension: Int): Double
    }

    val cubic: Geometry = new Geometry {

      val maxMagnitudeDecomposition: Splitter = (v: Seq[Double]) => { //Max dot product on basis vectors and their negatives versions.
        val maxMagnitudeIndex = v.map(math.abs).zipWithIndex.maxBy(_._1)._2
        v.zipWithIndex.map { case (c, i) => if (i == maxMagnitudeIndex) c else 0 }
      }
      override val radialSplitter: Splitter = maxMagnitudeDecomposition
      override val borderNormalSplitter: Splitter = maxMagnitudeDecomposition

      override def space(dimension: Int): Double = 4 * 2*dimension //TODO Geometry as a GeometryFactory to set dimension at the beginning ?
    }

    //TODO simplex ?
  }

  def cellRadialAdjustment(geometry: Geometry, vector: Seq[Double]): Seq[Double] = {
    val (componentToKeep, remainderToAdjust) = geometry.radialSplit(vector)
    val sphericalRadius = norm(componentToKeep)
    val sphericalRadialDirection = normalize(componentToKeep)

    val (borderNormalComponent, _) = geometry.borderNormalSplit(remainderToAdjust)
    val centerToBorderProportion = norm(borderNormalComponent) / sphericalRadius

    val touchingBorderRemainder = remainderToAdjust * (1/centerToBorderProportion)
    val touchingBorder = componentToKeep + touchingBorderRemainder
    val maxAngle = touchingBorder angle sphericalRadialDirection

    val newVectorAngle = centerToBorderProportion * maxAngle

    val newRemainderLength = sphericalRadius * math.tan(newVectorAngle)
    val adjustedRemainder = remainderToAdjust toNorm newRemainderLength
    val adjustedVector = componentToKeep + adjustedRemainder

    if(adjustedVector.count(_.isNaN) == 0) adjustedVector else vector
  }

  def cellBorderParallelAdjustment(geometry: Geometry, vector: Seq[Double]): Seq[Double] = {
    val (componentToKeep, remainderToAdjust) = geometry.radialSplit(vector)
    val (borderNormalComponent, borderParallelComponent) = geometry.borderNormalSplit(remainderToAdjust)



    val adjustedRemainder = ???
    val adjustedVector = componentToKeep + adjustedRemainder

    adjustedVector
  }

  def nSphereSurface(n: Int, r: Double): Double = {
    import scala.math.{pow, Pi}
    if(n % 2 == 0) {
      //noinspection ReplaceToWithUntil
      pow(2, n/2 + 1) * pow(Pi, n/2) * pow(r, n) / (1 to n-1 by 2).map(_.toDouble).product
    } else {
      pow(Pi, (n+1)/2) * pow(r, n) / (1.0/2.0 * (1 to (n-1)/2).map(_.toDouble).product)
    }
  }

  def spacialAdjustment(geometry: Geometry, dimension: Int): Double = {
    math.pow(geometry.space(dimension) / nSphereSurface(dimension - 1, 1), 1.0/dimension)
  }

  def spacialAdjustedNormalization(geometry: Geometry, vector: Seq[Double]): Seq[Double] = {
    val dimension = vector.length
    val radius = {
      val (radialComponent, _) = geometry.radialSplit(vector)
      norm(radialComponent)
    }
    vector toNorm radius * spacialAdjustment(geometry, dimension)
  }

}
