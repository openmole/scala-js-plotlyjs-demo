package plotlyjs.demo.utils.graph.directed

import plotlyjs.demo.utils.graph.directed.Graph._

import scala.collection.immutable.HashMap
import scala.language.postfixOps

class Graph[A] private (_hashMap: HashMap[A, Set[A]] = HashMap[A, Set[A]]()) {

  private def hashMap = _hashMap

  def vertices: Set[A] = _hashMap.keySet

  def directSuccessorsOf(vertex: A): Set[A] = _hashMap(vertex)

  def directPredecessorsOf(vertex: A): Set[A] = _hashMap.filter(_._2 contains vertex).keySet

  def uniqueDirectSuccessorOf(vertex: A): A = {
    val dso = directSuccessorsOf(vertex)
    if(dso.size == 1) {
      dso.head
    } else {
      throw new Exception("not unique")
    }
  }

  def uniqueDirectPredecessorOf(vertex: A): A = {
    val dpo = directPredecessorsOf(vertex)
    if(dpo.size == 1) {
      dpo.head
    } else {
      throw new Exception("not unique")
    }
  }

  def successorsOf(vertex: A): Set[A] = {
    val directSuccessors = directSuccessorsOf(vertex)
    directSuccessors ++ directSuccessors.flatMap(successorsOf)
  }

  def predecessorsOf(vertex: A): Set[A] = {
    val directPredecessors = directPredecessorsOf(vertex)
    directPredecessors ++ directPredecessors.flatMap(predecessorsOf)
  }

  def terminalSuccessorsOf(vertex: A): Set[A] = {
    val directSuccessors = directSuccessorsOf(vertex)
    if(directSuccessors.isEmpty) {
      Set(vertex)
    } else {
      directSuccessors.flatMap(terminalSuccessorsOf)
    }
  }

  def terminalPredecessorsOf(vertex: A): Set[A] = {
    val directPredecessors = directPredecessorsOf(vertex)
    if(directPredecessors.isEmpty) {
      Set(vertex)
    } else {
      directPredecessors.flatMap(terminalPredecessorsOf)
    }
  }

  //def arrows: Set[(A, A)] = _hashMap.flatMap { case (vertex, heads) => heads.map(head => ((vertex, head), null)) }.keySet
  def arrows: Iterable[(A, A)] = _hashMap.flatMap[(A, A)] { case (vertex, heads) => heads.map((vertex, _)) }

  override def toString: String = "Graph(\n" + _hashMap.flatMap({ case (vertex, heads) =>
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



  def concat(graph: Graph[A]): Graph[A] = new Graph(Graph.concat(_hashMap, graph.hashMap))
  def ++(graph: Graph[A]): Graph[A] = concat(graph)

  def removeAll(graph: Graph[A]): Graph[A] = new Graph(Graph.removeKeys(_hashMap, graph.hashMap))
  def --(graph: Graph[A]): Graph[A] = removeAll(graph)

  def removeArrows(graph: Graph[A]): Graph[A] = new Graph(Graph.removeValues(_hashMap, graph.hashMap))

  def added(vertex: Vertex[A]): Graph[A] = concat(vertex.toGraph)
  def +(vertex: A): Graph[A] = added(vertex)

  def removed(vertex: Vertex[A]): Graph[A] = removeAll(vertex.toGraph)
  def -(vertex: A): Graph[A] = removed(vertex)

  def added(arrow: Arrow[A]): Graph[A] = concat(arrow.toGraph)
  def +(arrow: Arrow[A]): Graph[A] = added(arrow)

  def removed(arrow: Arrow[A]): Graph[A] = removeArrows(arrow.toGraph)
  def -(arrow: Arrow[A]): Graph[A] = removed(arrow)



  def branchRemoved(vertex: A): Graph[A] = {
    var graph = this
    directSuccessorsOf(vertex).foreach(successor => graph = graph.branchRemoved(successor))
    graph - vertex
  }

  def redirected(redirected: A, to: A): Graph[A] = {
    branchRemoved(redirected) ++ Graph.from(directPredecessorsOf(redirected).map(_ --> to))
  }



  def filter(pred: A => Boolean): Graph[A] = {
    val filtering = vertices.filter(pred)
    new Graph(_hashMap filter { case (vertex, _) => filtering contains vertex } map { case (vertex, heads) => (vertex, heads intersect filtering)})
  }

  def mapVertices[B](f: A => B): HashMap[A, B] = HashMap.from(vertices.map(vertex => (vertex, f(vertex))))

  def mapGraph[B](f: A => B): Graph[B] = {
    val mapping = mapVertices(f)
    new Graph(_hashMap map { case (vertex, heads) => (mapping(vertex), heads.map(mapping)) })
  }



  def replaced(Replaced: A, by: A): Graph[A] = mapGraph {
    case Replaced => by
    case vertex => vertex
  }

}

object Graph {

  type M[A] = HashMap[A, Set[A]]

  def concat[A](m1: M[A], m2: M[A]): M[A] = {
    val grown = m1 map { case (key, values) => (key, values ++ m2.getOrElse(key, Set())) }
    val shrunk = m2 filterNot { case (key, _) => grown contains key }
    grown ++ shrunk
  }

  def removeKeys[A](m1: M[A], m2: M[A]): M[A] = {
    (m1 -- m2.keys).map { case (key, values) => (key, values -- m2.keys) }
  }

  def removeValues[A](m1: M[A], m2: M[A]): M[A] = {
    m1.map { case (key, values) => (key, values -- m2.getOrElse(key, Set())) }
  }



  abstract class GraphElement[A] {
    def toGraph: Graph[A]
  }

  implicit class Vertex[A](vertex: A) extends GraphElement[A] {
    override def toGraph: Graph[A] = new Graph(HashMap(vertex -> Set()))
  }

  class Arrow[A](tail: A, head: A) extends GraphElement[A] {
    override def toGraph: Graph[A] = new Graph(HashMap(tail -> Set(head), head -> Set()))
  }
  implicit class ImplicitTail[A](tail: A) {
    def -->(head: A): Arrow[A] = new Arrow(tail, head)
  }

  def apply[A](elements: GraphElement[A]*): Graph[A] = {
    elements.map(_.toGraph).reduce(_ ++ _)
  }

  def from[A](elements: Set[GraphElement[A]]): Graph[A] = {
    apply[A](elements.toSeq: _*)
  }

  def fromVertices[A](vertices: Set[A]) = /*from(vertices.map(v => v))*/new Graph(HashMap.from(vertices.map((_, Set[A]()))))
  //TODO replace by from(vertices.map(v => v)) and test it.



  def group[A](set: Set[A], pred: (A, A) => Boolean): Iterable[Set[A]] = {
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

  def mainTest(args: Array[String]): Unit = {
    var graph = Graph(1, 2, 1 --> 2) ++ Graph(3 --> 4, 1 --> 3)
    println(graph.vertices)
    println(graph.arrows)
    println(graph.directSuccessorsOf(1))
    graph = graph.replaced(1, 0)
    println(graph.arrows)
  }

}
