package mcts.engines

import java.lang.System.currentTimeMillis

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

class RunPar[S, A: ClassTag, P](val solver: MonteCarlo[S, A, P], parallelism: Int) {

  // Run monte carlo search until cpuTime hits a certain value.
  def timedMCTS(duration: Duration, state: S): Node[A] = {
    val node     = new Node[A](0, 0, Map.empty, 0)
    val stopTime = currentTimeMillis + duration.toMillis
    val player   = solver.game.currentPlayer(state)

    @tailrec
    def go(n: Node[A]): Node[A] = {
      val newNode = solver.select(state, n, player)

      if (currentTimeMillis > stopTime)
        newNode
      else
        go(newNode)
    }

    (0 until parallelism).par
      .map(_ => go(node))
      .seq
      .reduce(Node.combine[A])
  }
}
