package haskell

import scala.collection.SortedMap

object StdLib {
  def unionWith[K: Ordering, V](m1: SortedMap[K, V], m2: SortedMap[K, V], op: (V, V) => V): SortedMap[K, V] = {
    val ret = SortedMap.newBuilder[K, V]

    (m1.keys ++ m2.keys).toSet.foreach { key: K =>
      (m1.get(key), m2.get(key)) match {
        case (Some(v1), Some(v2)) => ret += ((key, op(v1, v2)))
        case (Some(v1), None)     => ret += ((key, v1))
        case (None, Some(v2))     => ret += ((key, v2))
        case (None, None)         =>
      }
    }
    ret.result()
  }

}
