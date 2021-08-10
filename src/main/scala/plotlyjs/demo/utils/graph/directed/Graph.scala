package plotlyjs.demo.utils.graph.directed

import plotlyjs.demo.utils.Utils.printCode
import plotlyjs.demo.utils.graph.directed.weighted.GraphMap.VertexAndWeight

import scala.collection.immutable.HashMap

class Graph[V](_wg: weighted.Graph[V, Null]) {

  type Vertex = Graph.Vertex[V]
  type Arrow = Graph.Arrow[V]

  private val wg = _wg

  def vertices: Set[V] = wg.vertices

  def arrows: Iterable[Arrow] = wg.arrows.map(arrow => Graph.Arrow(arrow.tail, arrow.head))

  def directSuccessorsOf(vertex: V): Set[V] = wg.directSuccessorsOf(vertex).keySet

  def directPredecessorsOf(vertex: V): Set[V] = wg.directPredecessorsOf(vertex).keySet

  def uniqueDirectSuccessorOf(vertex: V): V = wg.uniqueDirectSuccessorOf(vertex).vertex

  def uniqueDirectPredecessorOf(vertex: V): V = wg.uniqueDirectPredecessorOf(vertex).vertex

  def successorsOf(vertex: V): Set[V] = wg.successorsOf(vertex)

  def predecessorsOf(vertex: V): Set[V] = wg.predecessorsOf(vertex)

  def terminalSuccessorsOf(vertex: V): Set[V] = wg.terminalSuccessorsOf(vertex)

  def terminalPredecessorsOf(vertex: V): Set[V] = wg.terminalPredecessorsOf(vertex)

  override def toString: String = "Graph(\n" + wg.vertices.flatMap(vertex => {
    val heads = directSuccessorsOf(vertex)
    if(heads.isEmpty) {
      if(directPredecessorsOf(vertex).isEmpty) {
        Some(vertex)
      } else {
        None
      }
    } else {
      Some(heads.map(vertex + " --> " + _).mkString(", "))
    }
  }).map("  " + _).mkString(",\n") + "\n)"



  def concat(graph: Graph[V]): Graph[V] = new Graph(wg.concat(graph.wg))
  def ++(graph: Graph[V]): Graph[V] = concat(graph)

  def removeAll(graph: Graph[V]): Graph[V] = new Graph(wg.removeAll(graph.wg))
  def --(graph: Graph[V]): Graph[V] = removeAll(graph)

  def removeArrows(graph: Graph[V]): Graph[V] = new Graph(wg.removeArrows(graph.wg))

  def added(vertex: Vertex): Graph[V] = new Graph(wg.added(vertex.toWeightedVertex))
  def +(vertex: V): Graph[V] = added(vertex)

  def removed(vertex: Vertex): Graph[V] = new Graph(wg.removed(vertex.toWeightedVertex))
  def -(vertex: V): Graph[V] = removed(vertex)

  def added(arrow: Arrow): Graph[V] = new Graph(wg.added(arrow.toWeightedArrow))
  def +(arrow: Arrow): Graph[V] = added(arrow)

  def removed(arrow: Arrow): Graph[V] = new Graph(wg.removed(arrow.toWeightedArrow))
  def -(arrow: Arrow): Graph[V] = removed(arrow)



  def filterVertices(pred: V => Boolean): Graph[V] = new Graph(wg.filterVertices(pred))

  def verticesMapping[FV](f: V => FV): HashMap[V, FV] = wg.verticesMapping(f)

  def mapVertices[FV](f: V => FV): Graph[FV] = new Graph(wg.mapVertices(f))



  def replaced(Replaced: V, by: V): Graph[V] = new Graph(wg.replaced(Replaced, by))

}

object Graph {

  trait Element[V] {
    def toWeightedElement: weighted.Graph.Element
  }

  implicit class Vertex[V](vertex: V) extends Element[V] {
    def toWeightedVertex: weighted.Graph.Vertex[V] = weighted.Graph.Vertex(vertex)

    override def toWeightedElement: weighted.Graph.Element = toWeightedVertex
  }

  implicit class ImplicitTail[V](tail: V) {
    def -->(head: V): Arrow[V] = Arrow(tail, head)
  }
  case class Arrow[V](tail: V, head: V) extends Element[V] {
    def toWeightedArrow: weighted.Graph.Arrow[V, Null] = weighted.Graph.Arrow(tail, null, head)

    override def toWeightedElement: weighted.Graph.Element = toWeightedArrow
  }

  def apply[V](elements: Element[V]*): Graph[V] = new Graph(weighted.Graph.apply(elements.map(_.toWeightedElement): _*))

  def from[V](elements: Set[Element[V]]): Graph[V] = new Graph(weighted.Graph.from(elements.map(_.toWeightedElement)))

  def fromVertices[V](vertices: Set[V]): Graph[V] = new Graph(weighted.Graph.fromVertices(vertices))



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
    var graph = Graph("a", "b", "a" --> "b") ++ Graph("c" --> "d", "a" --> "c")
    println(graph)
    printCode(graph.directSuccessorsOf("a"))
    printCode(graph.directPredecessorsOf("a"))
    graph = graph.filterVertices(_ != "c")
    println(graph)
    graph = graph.replaced("a", "aa")
    println(graph)
  }

}
