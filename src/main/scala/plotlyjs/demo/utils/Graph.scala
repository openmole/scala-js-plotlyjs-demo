package plotlyjs.demo.utils

import plotlyjs.demo.utils.Graph.Arrow

import scala.collection.immutable.HashMap
import scala.language.postfixOps

class Graph[A] private (_hashMap: HashMap[A, Set[A]] = HashMap[A, Set[A]]()) {

  private def hashMap: Map[A, Set[A]] = _hashMap
  def vertices: Set[A] = _hashMap.keySet
  def headsOf(tail: A): Set[A] = _hashMap(tail)
  def arrows: Seq[(A, A)] = _hashMap.flatMap { case (vertex, heads) => heads.map((vertex, _)) }.toSeq

  def concat(graph: Graph[A]): Graph[A] = {
    val grownHashMap = _hashMap map { case (vertex, heads) =>
      var newHeads = heads
      if(graph.vertices contains vertex) {
        newHeads = newHeads ++ graph.headsOf(vertex)
      }
      (vertex, newHeads)
    }
    val shrankHashMap = graph.hashMap filterNot { case (vertex, _) =>
      grownHashMap contains vertex
    }
    new Graph(grownHashMap ++ shrankHashMap)
  }
  def ++(suffix: Graph[A]): Graph[A] = concat(suffix)

  def added(vertexToAdd: A): Graph[A] = this ++ new Graph(HashMap(vertexToAdd -> Set()))
  def +(vertex: A): Graph[A] = added(vertex)

  def removed(vertexToRemove: A): Graph[A] = new Graph((_hashMap - vertexToRemove) map { case (vertex, heads) => (vertex, heads - vertexToRemove) })
  def -(vertex: A): Graph[A] = removed(vertex)

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

  def filter(pred: A => Boolean): Graph[A] = {
    val filtering = vertices.filter(pred)
    new Graph(_hashMap filter { case (vertex, _) => filtering contains vertex } map { case (vertex, heads) => (vertex, heads intersect filtering)})
  }

  def mapVertices[B](f: A => B): HashMap[A, B] = HashMap.from(vertices.map(vertex => (vertex, f(vertex))))

  def mapGraph[B](f: A => B): Graph[B] = {
    val mapping = mapVertices(f)
    new Graph(_hashMap map { case (vertex, heads) => (mapping(vertex), heads.map(mapping)) })
  }

}

object Graph {

  type ElementType = String
  val VertexType : ElementType = "vertex"
  val ArrowType : ElementType = "arrow"

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

  def fromVertices[A](vertices: Set[A]) = new Graph(HashMap.from(vertices.map((_, Set[A]()))))

}
