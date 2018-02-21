package mcts

import mcts.solver.Node

import scala.concurrent.duration._
import renderers._
import advanced.ComposeGames._
import advanced.RepeatGame._

object Main {

  def main(args: Array[String]): Unit = {
    SolverInterface(games.TicTacToe.GameDef, 100.millis)(Runner.MapReduce(Node.combine))
    SolverInterface(games.ConnectFour.GameDef, 2000.millis)(Runner.MapReduce(Node.combine))

    val combined = compose(
      repeat(games.ConnectFour.GameDef, 2),
      compose(
        games.ConnectFour.GameDef,
        repeat(games.TicTacToe.GameDef, 3)
      )
    )
    SolverInterface(combined, 3000.millis)(Runner.MapReduce(Node.combine))

  }
}
