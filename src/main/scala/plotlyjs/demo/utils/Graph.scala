package plotlyjs.demo.utils

import scala.:+
import scala.collection.immutable.HashMap

class Graph[A](_hashMap: HashMap[A, Set[A]]) {

  def this(set: Set[A]) = this(HashMap.from(set.map((_, Set[A]()))))
  def this() = this(Set[A]())

  def hashMap: Map[A, Set[A]] = _hashMap
  def vertices: Set[A] = _hashMap.keySet
  def arrows: Seq[(A, A)] = _hashMap.flatMap[(A, A)] { case (vertex, heads) => heads.map((vertex, _)) }.toSeq

  def add(vertex: A): Graph[A] = new Graph(_hashMap + (vertex -> Set[A]()))

  def arrow(tail: A, head: A) = new Graph[A](_hashMap.map { case (vertex, heads) =>
    var newHeads = heads
    if(vertex == tail) newHeads = heads + head
    (vertex, newHeads)
  })

  def ++(suffix: Graph[A]): Graph[A] = new Graph[A](HashMap.from(_hashMap ++ suffix.hashMap))

  def filter(pred: A => Boolean): Graph[A] = {
    val filtering = vertices.filter(pred)
    new Graph[A](_hashMap filter { case (vertex, _) => filtering.contains(vertex) } map { case (vertex, heads) => (vertex, heads.intersect(filtering))})
  }

  def mapVertices[B](f: A => B): HashMap[A, B] = HashMap.from(vertices.map(vertex => (vertex, f(vertex))))

  def mapGraph[B](f: A => B): Graph[B] = {
    val mapping = mapVertices(f)
    new Graph[B](_hashMap.map { case (vertex, heads) => (mapping(vertex), heads.map(mapping)) })
  }

}
