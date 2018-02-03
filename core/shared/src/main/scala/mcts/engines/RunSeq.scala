package mcts.engines

import java.lang.System.currentTimeMillis

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

class RunSeq[State, @specialized(Int, AnyRef) Action: ClassTag, Player](
    val solver: MonteCarlo[State, Action, Player]
) {

  /* Run monte carlo search for a given duration */
  def timedMCTS(duration: Duration, state: State): Node[Action, Player] = {
    val node     = new Node[Action, Player](0, 0, debox.Map.empty, 0)
    val stopTime = currentTimeMillis + duration.toMillis
    val player   = solver.game.currentPlayer(state)

    @tailrec
    def go(n: Node[Action, Player]): Node[Action, Player] = {
      val newNode = solver.select(state, n, player)

      if (currentTimeMillis > stopTime)
        newNode
      else
        go(newNode)
    }

    go(node)
  }
}


