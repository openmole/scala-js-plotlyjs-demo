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

  def filterKeys[V, W](gm: GraphMap[V, W], pred: V => Boolean): GraphMap[V, W] = {
    val filtering = gm.keySet.filter(pred)
    gm
      .filter { case (vertex, _) => filtering contains vertex }
      .map {
        case (vertex, weightMap) => (
          vertex,
          weightMap.filter { case (head, _) => filtering contains head }
        )
      }
  }

  def keysMapping[V, W, FV](gm: GraphMap[V, W], f: V => FV): HashMap[V, FV] = HashMap.from(gm.keys.map(key => (key, f(key))))

  def mapKeys[V, W, FV](gm: GraphMap[V, W], f: V => FV): GraphMap[FV, W] = {
    val mapping = keysMapping(gm, f)
    HashMap.from(gm map {
      case (vertex, weightMap) => (
        mapping(vertex),
        weightMap.map { case (head, weight) => (mapping(head), weight) }
      )
    })
  }

}
