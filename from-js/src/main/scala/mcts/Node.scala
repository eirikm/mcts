package mcts

import scala.util.Random

case class Node[Player, Move](
    game:       Game[Player, Move],
    parentOpt:  Option[Node[Player, Move]],
    moveOpt:    Option[Move],
    var wins:   Int,
    var visits: Int
) {

  def childWithMove(game: Game[Player, Move])(move: Move) =
    Node(game, Some(this), Some(move), 0, 0)

  lazy val children: Iterable[Node[Player, Move]] = {
    val newGame = moveOpt.fold(game)(move => game.withMove(move))
    newGame.possibleMoves.map(childWithMove(newGame))
  }

  override def toString: String =
    s"[A: $moveOpt; " +
      s"W/V: $wins/$visits} = ${wins.toDouble / visits}; " +
      s"C: ${children.size}"

}

object Node {
  def NodeOrdering[Player, Move](random: Random): Ordering[Node[Player, Move]] = {
    val base: Ordering[Node[Player, Move]] =
      Ordering.by[Node[Player, Move], Double] {
        case n if n.game.isRandom =>
          random.nextInt(2)

        // always visit unvisited nodes first
        case n if n.visits == 0 =>
          Double.PositiveInfinity

        case node @ Node(_, Some(parent), _, _, _) =>
          // See https://en.wikipedia.org/wiki/Monte_Carlo_tree_search#Exploration_and_exploitation
          (node.wins / node.visits) + Math.sqrt(2 * Math.log(parent.visits) / node.visits)
        case _ =>
          0
      }

    (x1, x2) =>
      base.compare(x1, x2) match {
        // we want equal nodes to be chosen randomly
        case 0     => random.nextInt(2)
        case other => other
      }
  }
}
