package mcts

import scala.annotation.tailrec
import scala.language.higherKinds
import scala.util.Random

class MCTS[Player, Move](var game: Game[Player, Move], rounds: Int, random: Random) {
  private val NodeOrdering = Node.NodeOrdering[Player, Move](random)

  @tailrec
  private def select(node: Node[Player, Move]): Node[Player, Move] = {
    node.visits += 1
    node.children match {
      case Nil      => node
      case children => select(children.max(NodeOrdering))
    }
  }

  @tailrec
  private def backPropagate(node: Node[Player, Move], winner: Option[Player]): Unit = {
    winner match {
      case Some(node.game.currentPlayer) => node.wins += 1
      case Some(other)                   => ()
      case None                          => ()
    }
    node.parentOpt match {
      case Some(parent) => backPropagate(parent, winner)
      case None         =>
    }
  }

  def selectMove: Option[Move] = {
    var round = 0

    val rootNode =
      new Node[Player, Move](game, None, None, 0, 0)

    while (round < rounds) {
      val currentNode = select(rootNode)
      backPropagate(currentNode, currentNode.game.winner)

      round += 1
    }

    if (rootNode.children.isEmpty) None
    else rootNode.children.maxBy(_.visits).moveOpt
  }

  def performMove(move: Move): Unit =
    game = game.withMove(move)
}
