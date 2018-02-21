package mcts.engines

import mcts.Util

final class Node[Action](
    val numWins:    Int,
    val numPlays:   Int,
    val children:   Map[Action, Node[Action]],
    val lastPayout: Int
) {

  def withChildNode(action: Action, newChild: Node[Action]): Node[Action] = {
    val newChildren = children.updated(action, newChild)

    new Node(
      children   = newChildren,
      numWins    = numWins + lastPayout,
      numPlays   = numPlays + 1,
      lastPayout = newChild.lastPayout
    )
  }

  def bestAction: Action =
    Util.maxBy(children)(child => child.numWins.toDouble / child.numPlays)
}

object Node {

  /**
    * Observing that an immutable tree of `Node`s can be combined,
    *  enables us to perform the tree search in parallel.
    */
  def combine[A](x: Node[A], y: Node[A]): Node[A] =
    new Node(
      numWins    = x.numWins + y.numWins,
      numPlays   = x.numPlays + y.numPlays,
      children   = combineChildren(x.children, y.children),
      lastPayout = math.max(x.lastPayout, y.lastPayout)
    )

  private def combineChildren[A](xs: Map[A, Node[A]], ys: Map[A, Node[A]]): Map[A, Node[A]] = {
    val ret = Map.newBuilder[A, Node[A]]
    ret ++= xs

    ys.foreach {
      case (a, node) if xs.contains(a) =>
        ret += (a -> combine(xs(a), node))
      case (a, node) =>
        ret += (a -> node)
    }
    ret.result()
  }
}
