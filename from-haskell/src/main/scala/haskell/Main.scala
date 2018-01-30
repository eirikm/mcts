package haskell

import java.lang.System.currentTimeMillis
import scala.annotation.tailrec

object Main extends App {
  import scala.concurrent.duration._

  @tailrec
  def continue[S, A: Ordering, P: Ordering](spec: MCTS.Spec[S, A, P])(rand: StdGen, state: S): StdGen = {

    val mcts                 = new MCTS.MonteCarlo(spec)
    val (newRand, finalNode) = mcts.timedMCTS(10.millis, rand, state, MCTS.Node.empty[A, P])

    // Check for an inevitable tie.
    if (spec.actions(state).isEmpty) {
      println("It's a Tie!")
      newRand
    } else {
      val computerAction = mcts.bestAction(finalNode, state)
      println(s"${spec.player(state)} Chose: $computerAction")

      val nextState = spec.apply(computerAction, state)
      println(nextState)
      spec.winner(nextState) match {
        case Some(winner) =>
          println(s"$winner won")
          newRand
        case None => continue(spec)(newRand, nextState)
      }
    }
  }

  continue(TicTacToe.Game)(StdGen.simple(currentTimeMillis()), TicTacToe.EmptyState)
}
