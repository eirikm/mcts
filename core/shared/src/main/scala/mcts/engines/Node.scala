package mcts.engines

import mcts.Util

final class Node[@specialized(Int, AnyRef) Action, Player](
    val numWins:    Int,
    val numPlays:   Int,
    val children:   debox.Map[Action, Node[Action, Player]],
    val lastPayout: Int
) {

  def withChildNode(action: Action, newChild: Node[Action, Player]): Node[Action, Player] = {
    val newChildren = children.copy()
    newChildren(action) = newChild

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
  def combine[A, P](x: Node[A, P], y: Node[A, P]): Node[A, P] =
    new Node(
      numWins    = x.numWins + y.numWins,
      numPlays   = x.numPlays + y.numPlays,
      children   = combineChildren(x.children, y.children),
      lastPayout = math.max(x.lastPayout, y.lastPayout)
    )

  private def combineChildren[A, P](xs: debox.Map[A, Node[A, P]],
                                    ys: debox.Map[A, Node[A, P]]): debox.Map[A, Node[A, P]] = {
    val ret = xs.copy()

    ys.foreach {
      case (a, node) if ret.contains(a) =>
        ret(a) = combine(ret(a), node)
      case (a, node) =>
        ret(a) = node
    }
    ret
  }
}
