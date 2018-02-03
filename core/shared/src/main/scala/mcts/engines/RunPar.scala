package mcts.engines

import java.lang.System.currentTimeMillis

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

class RunPar[S, @specialized(Int, AnyRef) A: ClassTag, P](val solver: MonteCarlo[S, A, P], parallelism: Int) {

  // Run monte carlo search until cpuTime hits a certain value.
  def timedMCTS(duration: Duration, state: S): Node[A, P] = {
    val node     = new Node[A, P](0, 0, debox.Map.empty, 0)
    val stopTime = currentTimeMillis + duration.toMillis
    val player   = solver.game.currentPlayer(state)

    @tailrec
    def go(n: Node[A, P]): Node[A, P] = {
      val newNode = solver.select(state, n, player)

      if (currentTimeMillis > stopTime)
        newNode
      else
        go(newNode)
    }

    (0 until parallelism).par
      .map(_ => go(node))
      .seq
      .toList
      .reduce(Node.combine[A, P])
  }
}
