package plotlyjs.demo.utils.graph.directed

import plotlyjs.demo.utils.graph.directed.Graph2.Arrow
import plotlyjs.demo.utils.graph.directed.weighted.Graph
import plotlyjs.demo.utils.graph.directed.weighted.GraphMap.VertexAndWeight

import scala.collection.immutable.HashMap

class Graph2[V](_wg: weighted.Graph[V, Null]) {

  type Vertex = Graph2.Vertex[V]
  type Arrow = Graph2.Arrow[V]

  private def wg = _wg

  def vertices: Set[V] = wg.vertices

  def arrows: Iterable[Arrow] = wg.arrows.map(arrow => Arrow(arrow.tail, arrow.head))

  def directSuccessorsOf(vertex: V): Set[V] = wg.directSuccessorsOf(vertex).keySet

  def directPredecessorsOf(vertex: V): Set[V] = wg.directPredecessorsOf(vertex).keySet

  def uniqueDirectSuccessorOf(vertex: V): V = wg.uniqueDirectSuccessorOf(vertex).vertex

  def uniqueDirectPredecessorOf(vertex: V): V = wg.uniqueDirectPredecessorOf(vertex).vertex

  def successorsOf(vertex: V): Set[V] = wg.successorsOf(vertex)

  def predecessorsOf(vertex: V): Set[V] = wg.predecessorsOf(vertex)

  def terminalSuccessorsOf(vertex: V): Set[V] = wg.terminalSuccessorsOf(vertex)

  def terminalPredecessorsOf(vertex: V): Set[V] = wg.terminalPredecessorsOf(vertex)

  override def toString: String = "Graph(\n" + wg.gm.flatMap { case (vertex, heads) =>
    if(heads.isEmpty) {
      if(directPredecessorsOf(vertex).isEmpty) {
        Some(vertex)
      } else {
        None
      }
    } else {
      Some(heads.keySet.map(vertex + " --> " + _).mkString(", "))
    }
  }.map("  " + _).mkString(",\n") + "\n)"



  def concat(graph: Graph2[V]): Graph2[V] = new Graph2(wg.concat(graph.wg))
  def ++(graph: Graph2[V]): Graph2[V] = concat(graph)

  def removeAll(graph: Graph2[V]): Graph2[V] = new Graph2(wg.removeAll(graph.wg))
  def --(graph: Graph2[V]): Graph2[V] = removeAll(graph)

  def removeArrows(graph: Graph2[V]): Graph2[V] = new Graph2(wg.removeArrows(graph.wg))

  def added(vertex: Vertex): Graph2[V] = new Graph2(wg.added(vertex.toWeightedVertex))
  def +(vertex: V): Graph2[V] = added(vertex)

  def removed(vertex: Vertex): Graph2[V] = new Graph2(wg.removed(vertex.toWeightedVertex))
  def -(vertex: V): Graph2[V] = removed(vertex)

  def added(arrow: Arrow): Graph2[V] = new Graph2(wg.added(arrow.toWeightedArrow))
  def +(arrow: Arrow): Graph2[V] = added(arrow)

  def removed(arrow: Arrow): Graph2[V] = new Graph2(wg.removed(arrow.toWeightedArrow))
  def -(arrow: Arrow): Graph2[V] = removed(arrow)



  def filterVertices(pred: V => Boolean): Graph2[V] = new Graph2(wg.filterVertices(pred))

  def verticesMapping[FV](f: V => FV): HashMap[V, FV] = wg.verticesMapping(f)

  def mapVertices[FV](f: V => FV): Graph2[FV] = new Graph2(wg.mapVertices(f))



  def replaced(Replaced: V, by: V): Graph2[V] = new Graph2(wg.replaced(Replaced, by))

}

object Graph2 {

  trait ToGraph extends weighted.Graph.ToGraph

  implicit class Vertex[V](vertex: V) extends ToGraph {
    def toWeightedVertex: Graph.Vertex[V] = weighted.Graph.Vertex(vertex)

    override def toGraph[V, W]: Graph[V, W] = toWeightedVertex.toGraph
  }

  implicit class ImplicitTail[V](tail: V) {
    def -->(head: V): Arrow[V] = Arrow(tail, head)
  }
  case class Arrow[V](tail: V, head: V) extends ToGraph {
    def toWeightedArrow: Graph.Arrow[V, Null] = weighted.Graph.Arrow(tail, null, head)

    override def toGraph[V, W]: Graph[V, W] = toWeightedArrow.toGraph
  }

  def apply[V](elements: ToGraph*): Graph2[V] = new Graph2(weighted.Graph.apply(elements: _*))

  def from[V](elements: Set[ToGraph]): Graph2[V] = new Graph2(weighted.Graph.from(elements.asInstanceOf[Set[Graph.ToGraph]]))

  def fromVertices[V](vertices: Set[V]): Graph2[V] = new Graph2(weighted.Graph.fromVertices(vertices))



  def group[A](set: Set[A], pred: (A, A) => Boolean): Iterable[Set[A]] = {
    var graph = fromVertices(set)
    val seq = set.toSeq
    for(i1 <- 0 until seq.size - 1) {
      val v1 = seq(i1)
      for(i2 <- i1 + 1 until seq.size) {
        val v2 = seq(i2)
        if(pred(v1, v2)) {
          graph = graph + (graph.terminalSuccessorsOf(v1).head --> graph.terminalSuccessorsOf(v2).head)
        }
      }
    }
    graph.vertices.groupBy(graph.terminalSuccessorsOf).values
  }



  def test(): Unit = {
    var graph: Graph2[String] = Graph2("a", "b", "a" --> "b") ++ Graph2("c" --> "d", "a" --> "c")
    println(graph)
    println(graph.directSuccessorsOf("a"))
    graph = graph.filterVertices(_ != "c")
    println(graph)
    graph = graph.replaced("a", "aa")
    println(graph)
  }

}
