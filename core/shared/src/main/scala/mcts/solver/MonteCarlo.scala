package mcts.solver

import mcts._

import scala.annotation.tailrec
import scala.util.Random

/**
  * Monte Carlo Tree Search solver
  *
  * Loosely based on https://github.com/avinashbot/gamesearch
  *
  * @tparam State  state of the game
  * @tparam Action actions that can be done in a round in the game
  * @tparam Player players that are playing the game
  */
class MonteCarlo[State, Action, Player](val game: Game[State, Action, Player]) {

  def select(currentState: State, currentNode: Node[Action], player: Player): Node[Action] =
    game.gameResult(currentState) match {

      case gameEnded: GameEnded[Player] =>
        val thisPayout = game.payout(gameEnded, player)

        new Node(
          numWins    = currentNode.numWins + thisPayout,
          numPlays   = currentNode.numPlays + 1,
          children   = currentNode.children,
          lastPayout = thisPayout
        )

      case Ongoing(actions) =>
        /* Look for an unexpanded action to start simulation from. */
        val unexpandedActionOpt: Option[Action] =
          actions.find(a => !currentNode.children.contains(a))

        unexpandedActionOpt match {

          /* Perform expansion by creating a new node and simulate from there */
          case Some(actionToExpand) =>
            val nextState: State =
              game.nextState(actionToExpand, currentState)

            val gameEnded: GameEnded[Player] =
              simulateUntilGameEnds(nextState)

            val lastPayout = game.payout(gameEnded, player)

            val expanded = new Node[Action](
              numWins    = lastPayout,
              numPlays   = 1,
              children   = Map.empty,
              lastPayout = lastPayout
            )

            currentNode.withChildNode(action = actionToExpand, newChild = expanded)

          /* Recursively call select on a child of this tree chosen with UCB. */
          case None =>
            val isOpponentsTurn: Boolean =
              game.currentPlayer(currentState) != player

            val bestAction: Action =
              Util.maxBy(currentNode.children)(childNode => ucb(currentNode, childNode, isOpponentsTurn))

            val bestActionNode: Node[Action] =
              currentNode.children(bestAction)

            val nextState: State =
              game.nextState(bestAction, currentState)

            val updatedActionNode: Node[Action] =
              select(nextState, bestActionNode, player)

            currentNode.withChildNode(action = bestAction, newChild = updatedActionNode)
        }
    }

  @tailrec
  private def simulateUntilGameEnds(state: State): GameEnded[Player] =
    game.gameResult(state) match {
      case gameEnded: GameEnded[Player] =>
        gameEnded

      case Ongoing(actions) =>
        val randomAction: Action =
          actions(Random.nextInt(actions.length))

        val stateAfterAction: State =
          game.nextState(randomAction, state)

        simulateUntilGameEnds(stateAfterAction)
    }

  /**
    * Upper Confidence Bound 1 applied to trees
    */
  private def ucb(parent: Node[Action], child: Node[Action], isOpponentsTurn: Boolean): Double = {
    val numWins      = if (isOpponentsTurn) -child.numWins else child.numWins
    val exploitation = numWins.toDouble / child.numPlays
    val exploration  = Epsilon * math.sqrt(math.log(parent.numPlays) / child.numPlays)

    exploitation + exploration
  }

  private val Epsilon = math.sqrt(2)
}
