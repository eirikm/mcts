package mcts.advanced

import fansi.Str
import mcts._
import mcts.games.{Blue, Red, RedBlue}

object RepeatGame {
  case class Repeated[S](games: List[S], n: Int)

  def repeat[S, A](game: Game[S, A, RedBlue], numWins: Int): Game[Repeated[S], A, RedBlue] =
    new Game[Repeated[S], A, RedBlue] {
      override def payout(gameEnded: GameEnded[RedBlue], player: RedBlue): Int =
        game.payout(gameEnded, player) * numWins

      override val startingState: Repeated[S] =
        Repeated(List(game.startingState), numWins)

      override def currentPlayer(state: Repeated[S]): RedBlue =
        game.currentPlayer(state.games.head)

      override def gameResult(repeatState: Repeated[S]): GameResult[A, RedBlue] = {
        var red  = 0
        var blue = 0
        var idx  = 0
        while (idx < repeatState.games.length) {
          game.gameResult(repeatState.games(idx)) match {
            case Winner(Red)  => red += 1
            case Winner(Blue) => blue += 1
            case _            =>
          }
          idx += 1
        }

        if (red == numWins)
          Winner(Red)
        else if (blue == numWins)
          Winner(Blue)
        else
          game.gameResult(repeatState.games.head)
      }

      override def nextState(action: A, repeatState: Repeated[S]): Repeated[S] = {
        val newState = game.nextState(action, repeatState.games.head)
        game.gameResult(newState) match {
          case Ongoing(_) =>
            repeatState.copy(games = newState :: repeatState.games.tail)
          case _ =>
            val newGames: List[S] =
              if (repeatState.games.length != numWins)
                List(game.startingState, newState) ++ repeatState.games.tail
              else
                List(newState) ++ repeatState.games.tail

            repeatState.copy(games = newGames)
        }
      }
    }

  implicit def RenderRepeated[S](implicit render: Renderer[S, Str]): Renderer[Repeated[S], Str] =
    state => Str(s"First to ${state.n} wins:\n") ++ state.games.reverse.map(render.apply).reduce(_ ++ _)
}
