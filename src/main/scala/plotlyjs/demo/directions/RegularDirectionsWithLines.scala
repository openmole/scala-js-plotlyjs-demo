package plotlyjs.demo.directions

import plotlyjs.demo.directions.AngularAdjustment.Splitter._
import plotlyjs.demo.utils.Vectors._

import scala.collection.mutable
import scala.language.implicitConversions
import scala.math._

object RegularDirectionsWithLines {

  implicit class VectorRef(initialVector: Vector) {
    private var _vector = initialVector
    def read: Vector = _vector
    def write(newVector: Vector): Unit = _vector = newVector
    def apply(f: Vector => Vector): Unit = write(f(read))
    def copy: VectorRef = new VectorRef(read)
    private var removed = false
    def remove(): Unit = removed = true
    def isRemoved: Boolean = removed

    override def toString: String = read.toString
  }

  type LineSegment = (VectorRef, VectorRef)
  class VectorsAndLines(_vectors: Seq[VectorRef], _lineSegments: Seq[LineSegment] = Seq()) {
    def vectors: Seq[VectorRef] = _vectors
    def lineSegments: Seq[(VectorRef, VectorRef)] = _lineSegments
    def copy: VectorsAndLines = {
      val cleaned = clean
      val hashMap: mutable.HashMap[VectorRef, VectorRef] = mutable.HashMap()
      cleaned.vectors.foreach(v => hashMap += (v -> v.copy))
      val lineSegmentsCopy = cleaned.lineSegments.map(line => (hashMap(line._1), hashMap(line._2)))
      new VectorsAndLines(hashMap.values.toSeq, lineSegmentsCopy)
    }
    def ++(other: VectorsAndLines): VectorsAndLines = new VectorsAndLines(vectors ++ other.vectors, lineSegments ++ other.lineSegments)
    def clean = new VectorsAndLines(vectors.filter(!_.isRemoved), lineSegments.filter { case (v1Ref, v2Ref) => !v1Ref.isRemoved && !v2Ref.isRemoved })
  }

  def regularDirections(dim: Int, alphaStep: Double, sphericalShape: Boolean = false): VectorsAndLines = {
    val nSphereDim = dim - 1
    if(nSphereDim == 0) {
      new VectorsAndLines(Seq(Seq(-1.0), Seq(+1.0)), Seq())
    } else {
      val alphaMax = acos(1/sqrt(dim))

      var cell = if(alphaStep < alphaMax) {
        (1 to (alphaMax / alphaStep).toInt).map(i => {
          val rOnCell = tan(i * alphaStep)
          val rOnSphere = Seq(rOnCell, 1).normalize.head
          val cellSphere = regularDirections(nSphereDim, alphaStep / rOnSphere, sphericalShape = true)
          cellSphere.vectors.foreach(_.apply(scale(rOnCell)))
          cellSphere.vectors.foreach(vectorRef => {
            val componentNorm = maxMagnitudeComponent(vectorRef.read).norm
            if (componentNorm > 1) {
              vectorRef.apply(scale(1 / componentNorm))
              if (vectorRef.read.norm <= tan((i - 1) * alphaStep)) {
                vectorRef.remove()
              }
            }
          })
          cellSphere.vectors
            .filterNot(vectorRef => maxMagnitudeComponent(vectorRef.read).norm(1) <= 1)
            .foreach(_.remove())
          cellSphere.clean
        }).reduceLeft(_ ++ _)
      } else {
        new VectorsAndLines(Seq(), Seq())
      }

      val zeros: VectorRef = Seq.fill(nSphereDim)(0.0)
      //Adding lines
      if(nSphereDim == 1) {
        val vectorPairs = cell.vectors.grouped(2).toSeq
        var lineSegments = (for(i <- 0 until vectorPairs.length - 1) yield {
          if(vectorPairs(i + 1).length < 2) {
            Seq()
          } else {
            Seq(
              (vectorPairs(i)(0), vectorPairs(i + 1)(0)),
              (vectorPairs(i)(1), vectorPairs(i + 1)(1)),
            )
          }
        }).flatten
        if(vectorPairs.nonEmpty) lineSegments = lineSegments ++ Seq((zeros, vectorPairs(0)(0)), (zeros, vectorPairs(0)(1)))
        cell = new VectorsAndLines(cell.vectors, lineSegments)
      }
      println(cell.lineSegments)
      //

      cell = cell ++ new VectorsAndLines(Seq(zeros))

      val cubicNSphere = (0 to nSphereDim).map(insert => {
        Seq(Seq(-1.0), Seq(+1.0)).map(u => {
          val regularDirections = cell.copy
          regularDirections.vectors.foreach(_.apply(v => {
            val (vLeft, vRight) = v.splitAt(insert)
            vLeft ++ u ++ vRight
          }))
          regularDirections
        }).reduce(_ ++ _)
      }).reduce(_ ++ _) ++ new VectorsAndLines((0 until pow(2, dim).toInt).map(_.toBinaryString.toInt).map(s"%0${dim}d".format(_).map(c => if(c == '0') -1.0 else +1.0)))

      if(sphericalShape) cubicNSphere.vectors.foreach(_.apply(normalize))

      cubicNSphere
    }
  }

  def mainTest(args: Array[String]): Unit = {
    regularDirections(3, Pi/4 / 4)
  }

}
