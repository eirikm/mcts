package mcts.solver

import java.lang.System.currentTimeMillis

import scala.annotation.tailrec
import scala.concurrent.duration.Duration

class RunPar[State, Action, Player](val solver: MonteCarlo[State, Action, Player], parallelism: Int) {

  // Run monte carlo search until cpuTime hits a certain value.
  def timedMCTS(duration: Duration, state: State): Node[Action] = {
    val node     = new Node[Action](0, 0, Map.empty, 0)
    val stopTime = currentTimeMillis + duration.toMillis
    val player   = solver.game.currentPlayer(state)

    @tailrec
    def go(n: Node[Action]): Node[Action] = {
      val newNode = solver.select(state, n, player)

      if (currentTimeMillis > stopTime)
        newNode
      else
        go(newNode)
    }

    (0 until parallelism).par
      .map(_ => go(node))
      .seq
      .reduce(Node.combine[Action])
  }
}
