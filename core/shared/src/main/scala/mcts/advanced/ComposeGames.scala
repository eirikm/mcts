package mcts.advanced

import cats.data.Ior
import cats.data.Ior.{both, left, right}
import fansi.Str
import mcts._

object ComposeGames {

  def compose[S1, A1, S2, A2, P](g1: Game[S1, A1, P], g2: Game[S2, A2, P]): Game[(S1, S2), Ior[A1, A2], P] =
    new Game[(S1, S2), Ior[A1, A2], P] {
      override def payout(gameEnded: GameEnded[P], player: P): Int =
        g1.payout(gameEnded, player) * g2.payout(gameEnded, player)

      // format: off
      override def gameResult(state: (S1, S2)): GameResult[Ior[A1, A2], P] =
        (g1.gameResult(state._1), g2.gameResult(state._2)) match {
          case (Ongoing(a1s)    , Ongoing(a2s))     => Ongoing(a1s.flatMap(a1 => a2s.map(a2 => both(a1, a2))))
          case (Ongoing(a1s)    , Draw | Winner(_)) => Ongoing(a1s.map(left))
          case (Draw | Winner(_), Ongoing(a2s))     => Ongoing(a2s.map(right))
          case (Draw            , Draw)             => Draw
          case (Winner(p)       , Draw)             => Winner(p)
          case (Draw            , Winner(p))        => Winner(p)
          case (Winner(p1)      , Winner(p2))       => if (p1 == p2) Winner(p1) else Draw
        }

      override def currentPlayer(state: (S1, S2)): P =
        g1.currentPlayer(state._1)

      override def nextState(action: Ior[A1, A2], state: (S1, S2)): (S1, S2) =
        (action, state) match {
          case (Ior.Left(a1)    , (sl, sr)) => (g1.nextState(a1, sl), sr)
          case (Ior.Right(a2)   , (sl, sr)) => (sl,                   g2.nextState(a2, sr))
          case (Ior.Both(a1, ar), (sl, sr)) => (g1.nextState(a1, sl), g2.nextState(ar, sr))
        }
      // format: on
      override def startingState: (S1, S2) =
        (g1.startingState, g2.startingState)
    }

  implicit def RenderComposed[S1, S2](implicit renderLeft: Renderer[S1, fansi.Str],
                                      renderRight: Renderer[S2, fansi.Str]): Renderer[(S1, S2), fansi.Str] =
    new Renderer[(S1, S2), fansi.Str] {
      override def apply(state: (S1, S2)): Str =
        state match {
          case (stateLeft, stateRight) =>
            val linesLeft  = Util.lines(renderLeft(stateLeft))
            val linesRight = Util.lines(renderRight(stateRight))
            val height     = linesLeft.length.max(linesRight.length)
            val widthLeft  = linesLeft.map(_.length).max
            val widthRight = linesRight.map(_.length).max

            def pad(lines: List[fansi.Str], height: Int, width: Int): List[fansi.Str] =
              lines.map(line => line ++ (" " * (width - line.length))).padTo(height, " " * width: fansi.Str)

            pad(linesLeft, height, widthLeft)
              .zip(Array.fill(height - 1)(" | "))
              .zip(pad(linesRight, height, widthRight))
              .map {
                case ((x1, x2), x3) => x1 ++ x2 ++ x3 ++ "\n"
              }
              .reduce(_ ++ _)

        }
    }
}
