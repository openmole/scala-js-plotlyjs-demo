package plotlyjs.demo.utils.graph.directed.weighted

import scala.collection.immutable.HashMap

object GraphMap {

  implicit class VertexAndWeight[V, W](vw: (V, W)) {
    val vertex: V = vw._1
    val weight: W = vw._2
  }

  private type WeightMap[V, W] = HashMap[V, W]
  private type GraphMap[V, W] = HashMap[V, WeightMap[V, W]]

  def concat[V, W](gm1: GraphMap[V, W], gm2: GraphMap[V, W]): GraphMap[V, W] = {
    val grown = gm1 map { case (key, values) => (key, values ++ gm2.getOrElse(key, Set())) }
    val shrunk = gm2 filterNot { case (key, _) => grown contains key }
    grown ++ shrunk
  }

  def removeKeys[V, W](gm1: GraphMap[V, W], gm2: GraphMap[V, W]): GraphMap[V, W] = {
    (gm1 -- gm2.keys).map { case (key, values) => (key, values -- gm2.keys) }
  }

  def removeValues[V, W](gm1: GraphMap[V, W], gm2: GraphMap[V, W]): GraphMap[V, W] = {
    gm1.map { case (key, values) => (key, values -- gm2.get(key).map(_.keys).getOrElse(Set())) }
  }

}
