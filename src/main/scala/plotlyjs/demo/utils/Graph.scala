package plotlyjs.demo.utils

import plotlyjs.demo.utils.Graph._

import scala.collection.immutable.HashMap
import scala.language.postfixOps

class Graph[A] private (_hashMap: HashMap[A, Set[A]] = HashMap[A, Set[A]]()) {

  private def hashMap: Map[A, Set[A]] = _hashMap

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

  def arrows: Set[(A, A)] = _hashMap.flatMap { case (vertex, heads) => heads.map(head => ((vertex, head), null)) }.keySet



  def concat(graph: Graph[A]): Graph[A] = {
    val grownHashMap = _hashMap map { case (vertex, heads) =>
      var newHeads = heads
      if(graph.vertices contains vertex) {
        newHeads = heads ++ graph.directSuccessorsOf(vertex)
      }
      (vertex, newHeads)
    }
    val shrunkHashMap = graph.hashMap filterNot { case (vertex, _) =>
      grownHashMap contains vertex
    }
    new Graph(grownHashMap ++ shrunkHashMap)
  }
  def ++(graph: Graph[A]): Graph[A] = concat(graph)

  def added(vertexToAdd: A): Graph[A] = this ++ new Graph(HashMap(vertexToAdd -> Set()))
  def +(vertex: A): Graph[A] = added(vertex)

  def removed(vertexToRemove: A): Graph[A] = new Graph((_hashMap - vertexToRemove) map { case (vertex, heads) => (vertex, heads - vertexToRemove) })
  def -(vertex: A): Graph[A] = removed(vertex)

  //TODO param should be a graph as for ++
  //or create a new method
  def removeAll(verticesToRemove: Set[A]): Graph[A] = new Graph((_hashMap -- verticesToRemove) map { case (vertex, heads) => (vertex, heads -- verticesToRemove) })
  def --(vertices: Set[A]): Graph[A] = removeAll(vertices)

  def added(arrow: Arrow[A]): Graph[A] = {
    val tail = arrow.tail
    val head = arrow.head
    this ++ new Graph(HashMap(tail -> Set(head), head -> Set[A]()))
  }
  def +(arrow: Arrow[A]): Graph[A] = added(arrow)

  def removed(arrow: Arrow[A]): Graph[A] = {
    val tail = arrow.tail
    val head = arrow.head
    new Graph(_hashMap map { case (vertex, heads) =>
      var newHeads = heads
      if(vertex == tail) newHeads = heads - head
      (vertex, newHeads)
    })
  }
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

  type ElementType = String
  val VertexType: ElementType = "vertex"
  val ArrowType: ElementType = "arrow"

  class GraphElement[A](_elementType: ElementType) {
    def elementType: ElementType = _elementType
  }

  implicit class Vertex[A](_vertex: A) extends GraphElement[A](VertexType) {
    def vertex: A = _vertex
  }

  class Arrow[A](_tail: A, _head: A) extends GraphElement[A](ArrowType) {
    def tail: A = _tail
    def head: A = _head
  }
  implicit class ImplicitTail[A](tail: A) {
    def -->(head: A): Arrow[A] = new Arrow(tail, head)
  }

  def apply[A](elements: GraphElement[A]*): Graph[A] = {
    var graph = new Graph[A]
    elements.foreach(elem => {
      elem.elementType match {
        case VertexType => graph = graph + elem.asInstanceOf[Vertex[A]].vertex
        case ArrowType => graph = graph + elem.asInstanceOf[Arrow[A]]
      }
    })
    graph
  }

  def from[A](elements: Set[GraphElement[A]]): Graph[A] = {
    var graph = new Graph[A]
    elements.foreach(elem => {
      elem.elementType match {
        case VertexType => graph = graph + elem.asInstanceOf[Vertex[A]].vertex
        case ArrowType => graph = graph + elem.asInstanceOf[Arrow[A]]
      }
    })
    graph
  }

  def fromVertices[A](vertices: Set[A]) = new Graph(HashMap.from(vertices.map((_, Set[A]()))))



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
