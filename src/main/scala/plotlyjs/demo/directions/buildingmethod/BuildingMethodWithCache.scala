package plotlyjs.demo.directions.buildingmethod

import plotlyjs.demo.directions.angularadjustment.AngularAdjustment.Splitter._
import plotlyjs.demo.utils.graph.directed.Graph._
import plotlyjs.demo.utils.Vectors._
import plotlyjs.demo.utils.graph.directed.Graph

import scala.math._

object BuildingMethodWithCache {

  case class RecursionCall(dim: Int, angleStep: Double, rOnCell: Double = 1, previousROnCell: Double = Double.NaN, nSphere: Seq[Vector] = Seq()) {

    def resultHolder: RecursionCall = RecursionCall(dim, angleStep, rOnCell, previousROnCell, Seq())

    private def withUnitNSphere(unitNSphere: Seq[Vector]) = RecursionCall(dim, angleStep, rOnCell, previousROnCell, unitNSphere.map(scale(rOnCell)))

    def resolve(dependencies: Graph[RecursionCall], cache: Graph[RecursionCall], results: Graph[RecursionCall]): (Graph[RecursionCall], Graph[RecursionCall]) = {
      if (cache.uniqueDirectSuccessorOf(this).nSphere.nonEmpty) {
        (cache, results + (this --> withUnitNSphere(cache.uniqueDirectSuccessorOf(this).nSphere)))
      } else {
        val nSphereDim = dim - 1
        if (nSphereDim == 0) {
          val nSphere = Seq(Seq(-1.0), Seq(+1.0))
          (cache.replaced(cache.uniqueDirectSuccessorOf(this), cache.uniqueDirectSuccessorOf(this).withUnitNSphere(nSphere)), results + (this --> withUnitNSphere(nSphere)))
        } else {

          var updatedCache = cache
          var updatedResults = results

          dependencies.directSuccessorsOf(this).diff(results.vertices).foreach(unresolvedDependency => {
            val (dependencyUpdatedCache, dependencyUpdatedResults) = unresolvedDependency.resolve(dependencies, updatedCache, updatedResults)
            updatedCache = dependencyUpdatedCache
            updatedResults = dependencyUpdatedResults
          })

          val cell = dependencies.directSuccessorsOf(this).flatMap(dependency => {
            val sphere = updatedResults.uniqueDirectSuccessorOf(dependency).nSphere
            val maxMagnitudes = sphere.map(MaxMagnitudeComponent(_).norm)
            val inside = (sphere zip maxMagnitudes)
              .filter(_._2 <= 1).map(_._1)
            val border = (sphere zip maxMagnitudes)
              .filter(_._2 > 1)
              .map { case (v, m) => (1 / m) * v }
              .filter(_.norm > dependency.previousROnCell)
            inside ++ border
          }) ++ Seq(Seq.fill(dim - 1)(0.0))

          val cubicNSphere = cell.flatMap(v => {
            (0 to dim).flatMap(insert => {
              val (vLeft, vRight) = v.splitAt(insert)
              Seq(Seq(-1.0), Seq(+1.0)).map(u => {
                vLeft ++ u ++ vRight
              })
            })
          }) ++ (0 until pow(2, dim).toInt).map(_.toBinaryString.toInt).map(s"%0${dim}d".format(_).map(c => if (c == '0') -1.0 else +1.0))

          val sphericalNSphere = cubicNSphere.map(normalize).toSeq

          (updatedCache.replaced(updatedCache.uniqueDirectSuccessorOf(this), updatedCache.uniqueDirectSuccessorOf(this).withUnitNSphere(sphericalNSphere)), updatedResults + (this --> withUnitNSphere(sphericalNSphere)))
        }
      }
    }

  }

  def recursionGraph(recursionCall: RecursionCall): Graph[RecursionCall] = {
    val dim = recursionCall.dim
    val angleStep = recursionCall.angleStep

    var graph = Graph(recursionCall)

    val nSphereDim = recursionCall.dim - 1
    if (nSphereDim == 0) {

    } else {
      val alphaMax = acos(1 / sqrt(dim))
      (1 to (alphaMax / angleStep).toInt).foreach(i => {
        val previousROnCell = tan((i - 1) * angleStep)
        val rOnCell = tan(i * angleStep)
        val rOnSphere = Seq(rOnCell, 1).normalize.head
        val recursionAlphaStep = angleStep / rOnSphere
        val recursionDim = nSphereDim
        val successorCall = RecursionCall(recursionDim, recursionAlphaStep, rOnCell, previousROnCell)
        graph = graph + (recursionCall --> successorCall) ++ recursionGraph(successorCall)
      })
    }
    graph
  }

  def substituteGroups(graph: Graph[RecursionCall], angleDiff: Double): Iterable[Set[RecursionCall]] = {
    graph.vertices.groupBy(_.dim).values.flatMap(dimGroup => {
      Graph.group(dimGroup, (v1: RecursionCall, v2: RecursionCall) => math.abs(v1.angleStep - v2.angleStep) < angleDiff)
    })
  }

  def substituteCache(substituteGroups: Iterable[Set[RecursionCall]]): Graph[RecursionCall] = {
    Graph.from(substituteGroups.flatMap(group => {
      val substitute = RecursionCall(group.head.dim, group.map(_.angleStep).sum / group.size)
      group.map(recursionCall => recursionCall --> substitute)
    }).toSet)
  }

  def nSphereCovering(dim: Int, angleStep: Double, angleDiff: Double): Seq[Vector] = {
    val rootCall = RecursionCall(dim, angleStep)
    val callGraph = recursionGraph(rootCall)
    val cache = substituteCache(substituteGroups(callGraph, angleDiff))
    val (_, resolvedResults) = rootCall.resolve(callGraph, cache, Graph[RecursionCall]())
    resolvedResults.uniqueDirectSuccessorOf(rootCall).nSphere
  }

  def mainTest(args: Array[String]): Unit = {
    val angleStep = Pi / 4 / 2
    for (dim <- 1 to 42) {
      Seq(0, Pi / 4 / 16).foreach(angleDiff => {
        println(dim, angleStep, angleDiff)
        val t0 = System.nanoTime
        val nSphere = nSphereCovering(dim, angleStep, angleDiff)
        println(nSphere.size)
        println((System.nanoTime - t0) / 1E9)
      })
      println()
    }
  }

}
