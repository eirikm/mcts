package bfs

import scala.collection.mutable

/**
  * Created by culim on 2/25/16.
  */
object BFS {
  type BFSNode = Int

  def search(nodes: Set[BFSNode],
             edges: Set[(BFSNode, BFSNode)],
             start: BFSNode,
             end: BFSNode): Array[BFSNode] = {

    val parentOf = mutable.Map.empty[BFSNode, BFSNode]
    val closed = mutable.Set.empty[BFSNode]
    val open = mutable.Queue(start)

    while (open.nonEmpty) {

      val current = open.dequeue()
      closed += current

      if (current == end) {
        var nextOpt = Option(current)
        val path = mutable.ListBuffer.empty[BFSNode]

        while (nextOpt.isDefined) {
          val next = nextOpt.get
          path += next
          nextOpt = parentOf.get(next)
        }

        return path.reverse.toArray
      }

      for ((`current`, neighbor) <- edges if nodes.contains(neighbor)) {
        if (!closed.contains(neighbor)) {
          open += neighbor
          parentOf(neighbor) = current
        }
      }
    }

    Array.empty
  }
}
