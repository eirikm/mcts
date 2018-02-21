package mcts

import mcts.solver.Node

import scala.concurrent.duration._
import renderers._

object Main {

  def main(args: Array[String]): Unit = {
    SolverInterface(games.TicTacToe.GameDef, 100.millis)(Runner.MapReduce(Node.combine))
    SolverInterface(games.ConnectFour.GameDef, 2000.millis)(Runner.MapReduce(Node.combine))
  }
}
