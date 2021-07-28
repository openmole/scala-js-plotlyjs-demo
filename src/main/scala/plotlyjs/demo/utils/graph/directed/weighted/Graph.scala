package plotlyjs.demo.utils.graph.directed.weighted

import plotlyjs.demo.utils.graph.directed.weighted.Graph._
import plotlyjs.demo.utils.graph.directed.weighted.GraphMap._

import scala.collection.immutable.HashMap
import scala.language.postfixOps

class Graph[V, W] private (_gm: HashMap[V, HashMap[V, W]] = HashMap[V, HashMap[V, W]]()) {

  type Vertex = Graph.Vertex[V]
  type Arrow = Graph.Arrow[V, W]

  private def gm = _gm

  def vertices: Set[V] = gm.keySet

  def arrows: Iterable[Arrow] = gm.flatMap { case (vertex, weightMap) => weightMap.map { case (head, weight) => Arrow(vertex, weight, head) } }

  def directSuccessorsOf(vertex: V): HashMap[V, W] = gm(vertex)

  def directPredecessorsOf(vertex: V): HashMap[V, W] = gm.flatMap { case (v, weightMap) => weightMap.get(vertex).map((v, _)) }

  def uniqueDirectSuccessorOf(vertex: V): (V, W) = {
    val dso = directSuccessorsOf(vertex)
    if(dso.size == 1) {
      dso.head
    } else {
      throw new Exception("not unique")
    }
  }

  def uniqueDirectPredecessorOf(vertex: V): (V, W) = {
    val dpo = directPredecessorsOf(vertex)
    if(dpo.size == 1) {
      dpo.head
    } else {
      throw new Exception("not unique")
    }
  }

  def successorsOf(vertex: V): Set[V] = {
    val directSuccessors = directSuccessorsOf(vertex).keySet
    directSuccessors ++ directSuccessors.flatMap(successorsOf)
  }

  def successorsOf(op: (W, W) => W)(vertex: V): HashMap[V, W] = {
    val directSuccessors = directSuccessorsOf(vertex)
    directSuccessors ++ directSuccessors.flatMap { case (v, w) => successorsOf(op)(v).map { case (sv, sw) => (sv, op(w, sw))} }
  }

  def predecessorsOf(vertex: V): Set[V] = {
    val directPredecessors = directPredecessorsOf(vertex).keySet
    directPredecessors ++ directPredecessors.flatMap(predecessorsOf)
  }

  def predecessorsOf(op: (W, W) => W)(vertex: V): HashMap[V, W] = {
    val directPredecessors = directPredecessorsOf(vertex)
    directPredecessors ++ directPredecessors.flatMap { case (v, w) => predecessorsOf(op)(v).map { case (pv, pw) => (pv, op(w, pw))} }
  }

  def terminalSuccessorsOf(vertex: V): Set[V] = {
    val directSuccessors = directSuccessorsOf(vertex).keySet
    if(directSuccessors.isEmpty) {
      Set(vertex)
    } else {
      directSuccessors.flatMap(terminalSuccessorsOf)
    }
  }

  def terminalPredecessorsOf(vertex: V): Set[V] = {
    val directPredecessors = directPredecessorsOf(vertex).keySet
    if(directPredecessors.isEmpty) {
      Set(vertex)
    } else {
      directPredecessors.flatMap(terminalPredecessorsOf)
    }
  }

  override def toString: String = "Graph(\n" + gm.flatMap({ case (vertex, weightMap) =>
    if(weightMap.isEmpty) {
      if(directPredecessorsOf(vertex).isEmpty) {
        Some(vertex)
      } else {
        None
      }
    } else {
      Some(weightMap.map { case (head, weight) => vertex + " --" + weight + "-> " + head}.mkString(", ")) //TODO use annotations to get methods names ?
    }
  }).map("  " + _).mkString(",\n") + "\n)"



  def concat(graph: Graph[V, W]): Graph[V, W] = new Graph(GraphMap.concat(gm, graph.gm))
  def ++(graph: Graph[V, W]): Graph[V, W] = concat(graph)

  def removeAll(graph: Graph[V, W]): Graph[V, W] = new Graph(GraphMap.removeKeys(gm, graph.gm))
  def --(graph: Graph[V, W]): Graph[V, W] = removeAll(graph)

  def removeArrows(graph: Graph[V, W]): Graph[V, W] = new Graph(GraphMap.removeValues(gm, graph.gm))

  def added(vertex: Vertex): Graph[V, W] = concat(vertex.toGraph[V, W])
  def +(vertex: V): Graph[V, W] = added(vertex)

  def removed(vertex: Vertex): Graph[V, W] = removeAll(vertex.toGraph[V, W])
  def -(vertex: V): Graph[V, W] = removed(vertex)

  def added(arrow: Arrow): Graph[V, W] = concat(arrow.toGraph)
  def +(arrow: Arrow): Graph[V, W] = added(arrow)

  def removed(arrow: Arrow): Graph[V, W] = removeArrows(arrow.toGraph)
  def -(arrow: Arrow): Graph[V, W] = removed(arrow)



  /*
  def filter(pred: V => Boolean): Graph[V, W] = {
    removeAll(from(vertices.filter(pred).map(v => v)))
  }

  def mapVertices[B](f: A => B): HashMap[A, B] = HashMap.from(vertices.map(vertex => (vertex, f(vertex))))

  def mapGraph[B](f: A => B): Graph[B] = {
    val mapping = mapVertices(f)
    new Graph(gm map { case (vertex, heads) => (mapping(vertex), heads.map(mapping)) })
  }



  def replaced(Replaced: A, by: A): Graph[V, W] = mapGraph {
    case Replaced => by
    case vertex => vertex
  }
  */

}

object Graph {

  trait ToGraph {
    def toGraph[V, W]: Graph[V, W]
  }

  implicit class Vertex[V](vertex: V) extends ToGraph {
    override def toGraph[V, W]: Graph[V, W] = {
      val vertex = this.vertex.asInstanceOf[V]
      new Graph(HashMap(vertex -> HashMap()))
    }
  }

  implicit class ImplicitTail[V](tail: V) {
    def --[W](weight: W) = new TailAndWeight(tail, weight)
  }
  class TailAndWeight[V, W](tail: V, weight: W) {
    def ->(head: V): Arrow[V, W] = Arrow(tail, weight, head)
  }
  case class Arrow[V, W](tail: V, weight: W, head: V) extends ToGraph {
    override def toGraph[V, W]: Graph[V, W] = {
      val tail = this.tail.asInstanceOf[V]
      val weight = this.weight.asInstanceOf[W]
      val head = this.head.asInstanceOf[V]
      new Graph(HashMap(tail -> HashMap(head -> weight), head -> HashMap()))
    }
  }

  def apply[V, W](elements: ToGraph*): Graph[V, W] = {
    elements.map(_.toGraph[V, W]).reduceOption(_ ++ _).getOrElse(new Graph(HashMap()))
  }

  def from[V, W](elements: Set[ToGraph]): Graph[V, W] = {
    apply(elements.toSeq: _*)
  }



  /*
  def group[A](set: Set[A], pred: (A, A) => Boolean, weight: (A, A) => W): Iterable[Set[A]] = {
    var graph = Graph.fromVertices(set)
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
  */

  def test(): Unit = {
    var graph: Graph[String, Int] = Graph("a", "b", "a" --1-> "b") ++ Graph("c" --2-> "d", "a" --3-> "c")
    println(graph.vertices)
    println(graph.arrows)
    println(graph.directSuccessorsOf("a"))
    //graph = graph.replaced(1, 0)
    println(graph.arrows)
  }

}
