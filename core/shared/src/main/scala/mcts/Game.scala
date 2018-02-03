package mcts

/**
  * A game specification is characterized by three types.
  *
  * Note that by abstracting over these types, we guarantee
  *  that the solver won't introspect them.
  *
  * @tparam State  Current state of game
  * @tparam Action The actions a player can perform
  * @tparam Player A player in the specified game
  */
trait Game[State, @specialized(Int, AnyRef) Action, Player] {
  def startingState: State

  def currentPlayer(state: State): Player

  def gameResult(state: State): GameResult[Action, Player]

  def nextState(action: Action, state: State): State

  def payout(gameEnded: GameEnded[Player], player: Player): Int =
    gameEnded match {
      case Winner(winner) => if (winner == player) 1 else -1
      case Draw           => 0
    }
}

/**
  * A game result will always be in one of these three states
  */
sealed trait GameResult[+Action, +Player]
sealed trait GameEnded[+Player] extends GameResult[Nothing, Player]

case class Ongoing[Action](actions: Array[Action]) extends GameResult[Action, Nothing]

case class Winner[+Player](winner: Player) extends GameEnded[Player]

case object Draw extends GameEnded[Nothing]
